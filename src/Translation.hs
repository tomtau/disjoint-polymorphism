{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Translation where

import           Control.Applicative              ((<|>))
import           Control.Monad.Trans.Maybe
import qualified Data.Text
import           Env
import           PrettyPrint                      (pprint)
import           Source.Syntax                    as S
import           Target.Syntax                    as T
import           Unbound.Generics.LocallyNameless


type TMonad = TcMonad S.TmName S.Type


ordinary :: S.Type -> Bool
ordinary (S.Arr _ _) = True
ordinary S.IntT = True
ordinary S.BoolT = True
ordinary _ = False


transType :: S.Type -> T.Type
transType S.IntT = T.IntT
transType S.BoolT = T.BoolT
transType (S.Arr a b) = T.Arr (transType a) (transType b)
transType (Inter a b) = T.Product (transType a) (transType b)
-- transType top


(<:) :: S.Type -> S.Type -> MaybeT TMonad T.Expr
(<:) S.IntT S.IntT = return $ T.elam ("x", T.IntT) (T.evar "x")
(<:) S.BoolT S.BoolT = return $ T.elam ("x", T.BoolT) (T.evar "x")
(<:) a@(S.Arr a1 a2) (S.Arr b1 b2) = do
  c1 <- b1 <: a1
  c2 <- a2 <: b2
  let body = T.App c2 (T.App (T.evar "f") (T.App c1 (T.evar "x")))
  return $ T.elam ("f", transType a) (T.elam ("x", transType b1) body)
(<:) t1 (S.Inter t2 t3) = do
  c1 <- t1 <: t2
  c2 <- t1 <: t3
  let vx = T.evar "x"
  return $ T.elam ("x", transType t1) (T.Pair (T.App c1 vx) (T.App c2 vx))
(<:) a@(S.Inter t1 t2) t3 =
  let
    f c i = return $ T.elam ("x", transType a) (T.App c (T.Project (T.evar "x") i))
    f1 = do c1 <- t1 <: t3
            f c1 1
    f2 = do c2 <- t2 <: t3
            f c2 2
  in
  if ordinary t3
    then f1 <|> f2
    else MaybeT $ return Nothing
(<:) _ _ = MaybeT $ return Nothing


disjoint :: S.Type -> S.Type -> Bool
disjoint (S.Inter a1 a2) b = disjoint a1 b && disjoint a2 b
disjoint a (S.Inter b1 b2) = disjoint a b1 && disjoint a b2
disjoint (S.Arr _ a) (S.Arr _ b) = disjoint a b
disjoint S.IntT (S.Arr _ _) = True
disjoint (S.Arr _ _) S.IntT = True
disjoint S.BoolT (S.Arr _ _) = True
disjoint (S.Arr _ _) S.BoolT = True
disjoint S.IntT S.BoolT = True
disjoint S.BoolT S.IntT = True
disjoint _ _ = False


translate :: S.Expr -> Either Data.Text.Text (S.Type, T.Expr)
translate = runTcMonad . trans


trans :: S.Expr -> TMonad (S.Type, T.Expr)
trans expr = case expr of
  (S.Var x) -> do
    t <- lookupTy x
    return (t, T.evar $ show x)
  (S.IntV v) -> return (S.IntT, T.IntV v)
  (S.BoolV v) -> return (S.BoolT, T.BoolV v)
  (S.Anno e t) -> do
    e' <- check e t
    return (t, e')
  (S.App f a) -> do
    (arr, f') <- trans f
    case arr of
      (S.Arr t1 t2) -> do
        a' <- check a t1
        return (t2, T.App f' a')
      _ -> throwStrErr $ pprint arr ++ " is not an arrow type"
  (S.PrimOp op e1 e2) -> do
    e1' <- check e1 S.IntT
    e2' <- check e2 S.IntT
    return (S.IntT, T.PrimOp op e1' e2')
  (S.Merge e1 e2) -> do
    (t1, e1') <- trans e1
    (t2, e2') <- trans e2
    if disjoint t1 t2
      then return (Inter t1 t2, T.Pair e1' e2')
      else throwStrErr $ pprint t1 ++ " and " ++ pprint t2 ++ " are not disjoint"
  (S.If p e1 e2) -> do
    p' <- check p S.BoolT
    (t1, e1') <- trans e1
    (t2, e2') <- trans e2
    if aeq t1 t2
      then return (t1, T.If p' e1' e2')
      else throwStrErr $ pprint t1 ++ " and " ++ pprint t2 ++ " must be the same type"
  (S.Let bnd) -> do
    ((x, Embed e), b) <- unbind bnd
    (et, e') <- trans e
    (t, b') <- extendCtx (x, et) (trans b)
    return (t, T.App (T.elam (show x, transType et) b') e')
  _ -> throwStrErr $ "Cannot infer " ++ pprint expr
  where
    check :: S.Expr -> S.Type -> TMonad T.Expr
    check (S.Lam bnd) (S.Arr t1 t2) = do
      (x, body) <- unbind bnd
      body' <- extendCtx (x, t1) (check body t2)
      return $ T.elam (show x, transType t1) body'
    check e t = do
      (t2, e') <- trans e
      if aeq t t2
        then return e'
        else do
          cf <- runMaybeT (t2 <: t)
          case cf of
            Just c -> return $ T.App c e'
            Nothing -> throwStrErr $ pprint e ++ " cannot be type " ++ pprint t
