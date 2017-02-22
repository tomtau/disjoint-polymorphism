{-# LANGUAGE FlexibleContexts, PatternGuards #-}

module Source.Typing
  ( tcModule
  ) where

import           Common
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Environment
import           Prelude hiding ((<$>))
import           PrettyPrint
import           Source.Subtyping
import           Source.Syntax
import qualified Target.CBN as TC
import qualified Target.Syntax as T
import           Text.PrettyPrint.ANSI.Leijen hiding (Pretty)
import           Unbound.LocallyNameless


-- Type check a module
tcModule :: Module -> TcMonad (Type, T.UExpr, TC.Env)
tcModule m = do
  let decls = moduleEntries m
  let mainE = mainExpr m
  -- Eliminate type dependencies
  -- Note: after parsing, earlier declarations appear first in the list
  let tydecls =
        foldr
          (\(TyDef n p t) ds -> (TyDef n p (substs (toSubst ds) t)) : ds)
          []
          (reverse ([decl | decl@(TyDef _ _ _) <- decls]))
  let substPairs = toSubst tydecls
  -- Resolve type declarations
  let tmdecls =
        map
          (\(TmDef n p t) -> TmDef n (substs substPairs p) (substs substPairs t))
          [decl | decl@(TmDef _ _ _) <- decls]
  -- Check term declarations and produce target declarations
  targetDecls <- foldr tcE (return []) tmdecls
  -- Generate initial environment for execution
  let initEnv =
        foldl
          (\env (n, e) -> TC.extendCtx (n, e, env) env)
          TC.emptyEnv
          targetDecls
  -- Check main expression
  (typ, transE) <- local (extendCtxs tmdecls) $ infer (substs substPairs mainE)
  return (typ, transE, initEnv)
  where
    toSubst ds = [(n, t) | TyDef n _ t <- ds]
    tcE :: Decl -> TcMonad [(T.UName, T.UExpr)] -> TcMonad [(T.UName, T.UExpr)]
    tcE d m = do
      transD <- tcTmDecl d
      fmap (transD :) $ local (extendCtx d) m

-- Type check term declarations
tcTmDecl :: Decl -> TcMonad (T.UName, T.UExpr)
tcTmDecl (TmDef n typ term) = do
  oldDef <- lookupTmDef n
  case oldDef of
    Nothing -> do
      trans <- check term typ
      return (translate n, trans)
    Just _ -> throwError $ text "Multiple definitions of" <+>  text (show n)

tcTmDecl _ = throwError $ text "Not implemented"


---------------------------
-- Γ ⊢ e ⇒ A ~> E

-- note: target is untyped
---------------------------

infer :: Expr -> TcMonad (Type, T.UExpr)

{-

Γ ⊢ ⊤ ⇒ ⊤  ~> ()

-}
infer Top = return (TopT, T.UUnit)

infer (IntV n) = return (IntT, T.UIntV n)

infer (BoolV b) = return (BoolT, T.UBoolV b)

infer (StrV b) = return (StringT, T.UStrV b)

{-

   x:A ∈ Γ
---------------
Γ ⊢ x ⇒ A ~> x

-}
infer (Var x) = do
  t <- lookupTy x
  return (t, T.UVar (translate x))  -- Change the sort of a name

{-

Γ ⊢ e ⇐ A  ~> E
------------------
Γ ⊢ e : A ⇒ A ~> E

-}
infer (Anno e a) = do
  e' <- check e a
  return (a, e')

{-

Γ ⊢ e1 ⇒ A1 -> A2  ~> E1
Γ ⊢ e2 ⇐ A1        ~> E2
----------------------------
Γ ⊢ e1 e2 ⇒ A2     ~> E1 E2

-}
infer inp@(App e1 e2) = do
  (arr, e1') <- infer e1
  case arr of
    Arr a1 a2 -> do
      e2' <- check e2 a1
      return (a2, T.UApp e1' e2')
    _ ->
      throwError
        (hang 2 $
         text "type of application mismatch in" <+>
         squotes (pprint inp) <> colon <$>
         text "function" <+>
         squotes (pprint e1) <+> text "has type" <+> squotes (pprint arr))

{-

Γ ⊢ e ⇒ ∀(α ∗ B). C  ~> E
Γ ⊢ A
Γ ⊢ A ∗ B
-------------------------------
Γ ⊢ e A ⇒ [α := A] C  ~> E

-}
infer inp@(TApp e a) = do
  (t, e') <- infer e
  wf a
  case t of
    DForall t' -> do
      ((x, Embed b), c) <- unbind t'
      ctx <- ask
      disjoint ctx a b
      return (subst x a c, e')
    _ ->
      throwError
        (hang 2 $
         text "type of application mismatch in" <+>
         squotes (pprint inp) <> colon <$>
         text "type-level function" <+>
         squotes (pprint e) <+> text "has type" <+> squotes (pprint t))

{-

Γ ⊢ e1 ⇒ A ~> E1
Γ ⊢ e2 ⇒ B ~> E2
Γ ⊢ A∗B
------------------------------
Γ ⊢ e1,,e2 ⇒ A&B ~> (E1, E2)

-}
infer (Merge e1 e2) = do
  (a, e1') <- infer e1
  (b, e2') <- infer e2
  ctx <- ask
  disjoint ctx a b
  return (And a b, T.UPair e1' e2')

{-

Γ ⊢ e ⇒ A ~> E
----------------------
Γ ⊢{l=e} ⇒ {l:A} ~> E

-}
infer (DRec l e) = do
  (a, e') <- infer e
  return (SRecT l a, e')

{-

Γ ⊢ e ⇒ {l : A} ~> E
----------------------
Γ ⊢ e.l ⇒ A ~> E

The above is what is shown in the paper. In the implementation, we'd like to
avoid annotating a record before projection. The following is the modified rule:

Γ ⊢ e ⇒ t ~> E
t contains l with {l : A}
t <: {l : A} ~> c
-----------------------
Γ ⊢ e.l ⇒ A ~> c E

-}
infer (Acc e l) = do
  (t, e') <- infer e
  case findLabel l t of
    Just t'@(SRecT _ a) -> do
      case t <<: t' of
        Left err ->
          throwError
            (hang 2 $
             text "record projection failed in" <+> squotes (pprint e) <> colon <$>
             text "it has type" <+>
             squotes (pprint t) <+>
             text "which is not a subtype of" <+> squotes (pprint t'))
        Right c -> return (a, T.UApp c e')
    _ ->
      throwError
        (hang 2 $
         text "expect a record type with label" <+>
         squotes (text l) <+> text "for" <+> squotes (pprint e) <$>
         text "but got" <+> squotes (pprint t))

{-

Γ ⊢ A
Γ , a * A ⊢ e ⇒ B ~> E
a fresh
---------------------------------
Γ ⊢ Λ(α∗A).e ⇒ ∀(α∗A).B ~> E

-}
infer (DLam t) = do
  ((x, Embed a), e) <- unbind t
  wf a
  (b, e') <- local (extendTyVarCtx x a) $ infer e
  return (DForall (bind (x, embed a) b), e')

infer (PrimOp op e1 e2) =
  case op of
    Arith _ -> do
      e1' <- check e1 IntT
      e2' <- check e2 IntT
      return (IntT, T.UPrimOp op e1' e2')
    Logical _ -> do
      e1' <- check e1 IntT
      e2' <- check e2 IntT
      return (BoolT, T.UPrimOp op e1' e2')

infer inp@(If e1 e2 e3) = do
  e1' <- check e1 BoolT
  (t2, e2') <- infer e2
  (t3, e3') <- infer e3
  if aeq t2 t3
    then return (t2, T.UIf e1' e2' e3')
    else throwError $
         (hang 2 $
          text "if branches type mismatch in" <+> squotes (pprint inp) <> colon <$>
          squotes (pprint e2) <+> text "has type" <+> squotes (pprint t2) <$>
          squotes (pprint e3) <+> text "has type" <+> squotes (pprint t3))

{-

Γ, x:t ⊢ e1 ⇐ t ~> e1'
Γ, x:t ⊢ e2 ⇒ t' ~> e2'
-----------------------------------------------------
Γ ⊢ let x : t = e1 in e2 ⇒ t' ~> let x = e1' in e2'

Note: Recursive let binding

-}
infer (Let b) = do
  ((x, Embed t, Embed e1), e2) <- unbind b
  e1' <- local (extendVarCtx x t) $ check e1 t
  (t', e2') <- local (extendVarCtx x t) $ infer e2
  return (t', T.ULet (bind (translate x) (e1', e2')))

infer a = throwError $ text "Infer not implemented:" <+> pprint a




------------------------
-- Γ ⊢ e ⇐ A ~> E
------------------------

check :: Expr -> Type -> TcMonad T.UExpr

{-

Γ ⊢ A
Γ , x:A ⊢ e ⇐ B ~> E
---------------------------
Γ ⊢ λx. e ⇐ A → B ~> λx. E

-}
check (Lam l) (Arr a b) = do
  (x, e) <- unbind l
  wf a
  e' <- local (extendVarCtx x a) $ check e b
  return (T.ULam (bind (translate x) e'))

check inp@(Lam _) t =
  throwError $
  text "expect an arrow type for " <+>
  squotes (pprint inp) <+> text "but got" <+> squotes (pprint t)


{-


Γ ⊢ A
Γ , a * A ⊢ e ⇐ B ~> E
---------------------------------
Γ ⊢ Λ(α∗A).e ⇐ ∀(α∗A).B ~> E


-}
check (DLam l) (DForall b) = do
  t <- unbind2 l b
  case t of
    Just ((x, Embed a), e, _, b) -> do
      wf a
      local (extendTyVarCtx x a) $ check e b
    Nothing -> throwError $ text "Patterns have different binding variables"

check inp@(DLam _) t =
  throwError $
  text "expect a forall type" <+>
  squotes (pprint inp) <+> text "but got" <+> squotes (pprint t)

{-

Γ ⊢ e ⇒ A ~> E
A <: B ~> c
Γ ⊢ B
-------------------
Γ ⊢ e ⇐ B ~> c E

-}

check e b = do
  (a, e') <- infer e
  let res = a <<: b
  case res of
    Right c -> do
      wf b
      return (T.UApp c e')
    Left err ->
      throwError
        (hang 2 $
         text "subtyping failed" <> colon <$>
         squotes (pprint e) <+> text "has type" <+> squotes (pprint a) <$>
         text "which is not a subtype of" <+> squotes (pprint b))


wf :: Type -> TcMonad ()
wf IntT = return ()
wf BoolT = return ()
wf StringT = return ()
wf (Arr a b) = wf a >> wf b
wf (And a b) = do
  wf a
  wf b
  ctx <- ask
  disjoint ctx a b
wf (TVar x) = lookupTyVar x >> return ()
wf (DForall t) = do
  ((x, Embed a), b) <- unbind t
  wf a
  local (extendTyVarCtx x a) $ wf b
wf (SRecT _ t) = wf t
wf TopT = return ()


-- Careful, am I following strictly Fig.3?
disjoint :: (Fresh m, MonadError Doc m) => Ctx -> Type -> Type -> m ()
disjoint _ TopT _ = return ()
disjoint _ _ TopT = return ()

disjoint ctx (TVar x) b
  | Just a <- lookupTyVarMaybe ctx x
  , Right _ <- a <<: b = return ()
disjoint ctx b (TVar x)
  | Just a <- lookupTyVarMaybe ctx x
  , Right _ <- a <<: b = return ()

disjoint ctx (DForall t) (DForall t') = do
  t <- unbind2 t t'
  case t of
    Just ((x, Embed a1), b, (y, Embed a2), c) ->
      disjoint (extendTyVarCtx x (And a1 a2) ctx) b c
    _ -> throwError $ text "Patterns have different binding variables"

disjoint ctx (SRecT l a) (SRecT l' b) =
  if l == l'
    then disjoint ctx a b
    else return ()

disjoint ctx (Arr _ a2) (Arr _ b2) = disjoint ctx a2 b2
disjoint ctx (And a1 a2) b = do
  disjoint ctx a1 b
  disjoint ctx a2 b
disjoint ctx a (And b1 b2) = do
  disjoint ctx a b1
  disjoint ctx a b2
disjoint _ IntT IntT = throwError $ text "Int and Int are not disjoint"
disjoint _ BoolT BoolT = throwError $ text "Bool and Bool are not disjoint"
disjoint _ StringT StringT = throwError $ text "String and String are not disjoint"
disjoint _ _ _ = return ()


findLabel :: Label -> Type -> Maybe Type
findLabel l IntT = Nothing
findLabel l BoolT = Nothing
findLabel l StringT = Nothing
findLabel l (Arr t1 t2) = Nothing
findLabel l (And t1 t2) = mplus (findLabel l t1) (findLabel l t2)
findLabel l (TVar _) = Nothing
findLabel l (DForall{}) = Nothing
findLabel l inp@(SRecT l' t) = if l == l' then Just inp else Nothing
findLabel l TopT = Nothing

-- transTyp :: Fresh m => Type -> m T.Type
-- transTyp IntT = return T.IntT
-- transTyp BoolT = return T.BoolT
-- transTyp (Arr t1 t2) = do
--   t1' <- transTyp t1
--   t2' <- transTyp t2
--   return (T.Arr t1' t2')
-- transTyp (And t1 t2) = do
--   t1' <- transTyp t1
--   t2' <- transTyp t2
--   return (T.Prod t1' t2')
-- transTyp (DForall t) = do
--   ((x, _), body) <- unbind t
--   b <- transTyp body
--   return (T.Forall (bind (translate x) b))
-- transTyp (SRecT _ t) = transTyp t
-- transTyp TopT = return T.UnitT
-- transTyp (TVar x) = return . T.TVar . translate $ x
