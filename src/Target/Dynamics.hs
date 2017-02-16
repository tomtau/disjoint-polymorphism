{-# LANGUAGE RecursiveDo #-}


module Target.Dynamics where


import Common
import Control.Monad.Reader
import Env
import Target.Syntax
import Unbound.LocallyNameless

-----------------------
-- System F -> untyped
-----------------------

erase :: Fresh m => Expr -> m UExpr
erase (Var x) = return $ UVar (translate x)
erase (App e1 e2) = do
  e1' <- erase e1
  e2' <- erase e2
  return $ UApp e1' e2'
erase (Lam b) = do
  (x, body) <- unbind b
  b' <- erase body
  return $ ULam (bind (translate x) b')
erase (BLam b) = do
  (_, body) <- unbind b
  b' <- erase body
  return b'
erase (TApp e _) = erase e
erase (Pair e1 e2) = do
  e1' <- erase e1
  e2' <- erase e2
  return (UPair e1' e2')
erase (Proj1 e) = do
  e' <- erase e
  return $ UP1 e'
erase (Proj2 e) = do
  e' <- erase e
  return $ UP2 e'
erase (IntV n) = return $ UIntV n
erase (BoolV n) = return $ UBoolV n
erase Unit = return UUnit
erase (PrimOp op e1 e2) = do
  e1' <- erase e1
  e2' <- erase e2
  return $ UPrimOp op e1' e2'
erase (If e1 e2 e3) = do
  e1' <- erase e1
  e2' <- erase e2
  e3' <- erase e3
  return $ UIf e1' e2' e3'
erase (Let t) = do
  (x, (e, body)) <- unbind t
  e' <- erase e
  b' <- erase body
  return $ ULet (bind (translate x) (e', b'))

------------------------
-- big-step evaluation
------------------------

type Env = Context UName Value

data Value = VInt Int
           | VBool Bool
           | VPair Value Value
           | VUnit
           | VClosure (Bind UName UExpr) Env

instance Show Value where
  show (VInt n) = show n
  show (VBool n) = show n
  show (VPair v1 v2) = "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
  show VUnit = "()"
  show _ = "Cannot show functions"


type EvalMonad = TcMonad UName Value

eval :: UExpr -> EvalMonad Value
eval (UVar x) = lookupTy x
eval (UApp e1 e2) = do
  VClosure b env' <- eval e1
  (x, body) <- unbind b
  v2 <- eval e2
  local (const env') $ extendCtx (x, v2) (eval body)
eval (ULam b) = do
  ctx <- ask
  return $ VClosure b ctx
-- Recursive let binding
eval (ULet b) = mdo
  (x, (e, body)) <- unbind b
  v <- extendCtx (x, v) $ eval e
  extendCtx (x, v) $ eval body
eval (UPair e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return $ VPair v1 v2
eval (UP1 e) = do
  VPair v1 _ <- eval e
  return v1
eval (UP2 e) = do
  VPair _ v2 <- eval e
  return v2
eval (UIntV n) = return $ VInt n
eval (UBoolV n) = return $ VBool n
eval UUnit = return VUnit
eval (UPrimOp op e1 e2) = do
  (VInt v1) <- eval e1
  (VInt v2) <- eval e2
  return $ evalOp op v1 v2
eval (UIf e1 e2 e3) = do
  (VBool v) <- eval e1
  if v then eval e2 else eval e3


evalOp :: Operation -> Int -> Int -> Value
evalOp op x y =
  case op of
    (Arith Add) -> VInt $ x + y
    (Arith Sub) -> VInt $ x - y
    (Arith Mul) -> VInt $ x * y
    (Arith Div) -> VInt $ x `div` y
    (Logical Equ) -> VBool $ x == y
    (Logical Neq) -> VBool $ x /= y
    (Logical Lt) -> VBool $ x < y
    (Logical Gt) -> VBool $ x > y
