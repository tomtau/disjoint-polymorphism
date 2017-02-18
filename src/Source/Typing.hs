
module Source.Typing
  ( tcModule
  ) where

import           Common
import           Control.Monad
import           Environment
import           PrettyPrint
import           Source.Subtyping
import           Source.Syntax
import qualified Target.Syntax as T
import qualified Target.CBN as TC
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
        foldl (\env (n, e) -> TC.extendCtx (n, e, env) env) [] targetDecls
  -- Check main expression
  (typ, transE) <- extendCtxs tmdecls $ infer (substs substPairs mainE)
  return (typ, transE, initEnv)
  where
    toSubst ds = [(n, t) | TyDef n _ (Just t) <- ds]
    tcE :: Decl -> TcMonad [(T.UName, T.UExpr)] -> TcMonad [(T.UName, T.UExpr)]
    tcE d m = do
      transD <- tcTmDecl d
      (transD :) <$> extendCtx d m

-- Type check declarations
tcTmDecl :: Decl -> TcMonad (T.UName, T.UExpr)
tcTmDecl t@(TmDef n typ (Just term)) = do
  oldDef <- lookupTmDef n
  case oldDef of
    Nothing -> do
      trans <- check term typ
      return (translate n, trans)
    Just _ -> throwStrErr $ "Multiple definitions of " ++ show n

tcTmDecl _ = throwStrErr $ "Not implemented"


---------------------------
-- Γ ⊢ e ⇒ A ~> E

-- note: target is untyped
---------------------------

infer :: Expr -> TcMonad (Type, T.UExpr)

{-

Γ ⊢ ⊤ ⇒ ⊤  ~> ()

-}
infer Top = return (TopT, T.UUnit)

{-

Γ ⊢ i ⇒ Int ~> i

-}
infer (IntV n) = return (IntT, T.UIntV n)

{-

Γ ⊢ b ⇒ bool ~> b

-}
infer (BoolV b) = return (BoolT, T.UBoolV b)

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
infer (App e1 e2) = do
  (arr, e1') <- infer e1
  case arr of
    Arr a1 a2 -> do
      e2' <- check e2 a1
      return (a2, T.UApp e1' e2')
    _ -> throwStrErr $ pprint arr ++ " is not an arrow type"

{-

Γ ⊢ e ⇒ ∀(α ∗ B). C  ~> E
Γ ⊢ A
Γ ⊢ A ∗ B
-------------------------------
Γ ⊢ e A ⇒ [α := A] C  ~> E

-}
infer (TApp e a) = do
  (t, e') <- infer e
  wf a
  case t of
    DForall t' -> do
      ((x, Embed b), c) <- unbind t'
      disjoint a b
      return (subst x a c, e')
    _ -> throwStrErr $ pprint t ++ " is not a quantifier"

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
  disjoint a b
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

-}
infer (Acc e l) = do
  (t, e') <- infer e
  case t of
    SRecT l' a ->
      if l == l'
        then return (a, e')
        else throwStrErr $ "labels not equal: " ++ l ++ " and " ++ l'
    _ -> throwStrErr $ pprint e ++ " is not a record"

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
  (b, e') <- extendCtx (TyDef x a Nothing) $ infer e
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

infer (If e1 e2 e3) = do
  e1' <- check e1 BoolT
  (t2, e2') <- infer e2
  (t3, e3') <- infer e3
  if aeq t2 t3
    then return (t2, T.UIf e1' e2' e3')
    else throwStrErr $ pprint t2 ++ " and " ++ pprint t3 ++ " must be the same type"

{-

Γ, x:t ⊢ e1 ⇐ t ~> e1'
Γ, x:t ⊢ e2 ⇒ t' ~> e2'
-----------------------------------------------------
Γ ⊢ let x : t = e1 in e2 ⇒ t' ~> let x = e1' in e2'

Note: Recursive let binding

-}
infer (Let b) = do
  ((x, Embed t, Embed e1), e2) <- unbind b
  e1' <- extendCtx (TmDef x t Nothing) $ check e1 t
  (t', e2') <- extendCtx (TmDef x t Nothing) $ infer e2
  return (t', T.ULet (bind (translate x) (e1', e2')))

infer a = throwStrErr $ "Infer not implemented: " ++ pprint a




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
  e' <- extendCtx (TmDef x a Nothing) $ check e b
  return (T.ULam (bind (translate x) e'))

check (Lam _) t = throwStrErr $ "lambda expects arrow type: " ++ pprint t


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
      extendCtx (TyDef x a Nothing) $ check e b
    Nothing -> throwStrErr $ "Patterns have different binding variables"

check (DLam _) t = throwStrErr $ "type-level lambda expects forall type: " ++ pprint t

{-

Γ ⊢ e ⇒ A ~> E
A <: B ~> c
Γ ⊢ B
-------------------
Γ ⊢ e ⇐ B ~> c E

-}
check e b = do
  (a, e') <- infer e
  c <- a <: b
  wf b
  return (T.UApp c e')


wf :: Type -> TcMonad ()
wf IntT = return ()
wf BoolT = return ()
wf (Arr a b) = wf a >> wf b
wf (And a b) = wf a >> wf b >> disjoint a b
wf (TVar x) = lookupTyVar x >> return ()
wf (DForall t) = do
  ((x, Embed a),b) <- unbind t
  wf a
  extendCtx (TyDef x a Nothing) $ wf b
wf (SRecT _ t) = wf t
wf TopT = return ()

disjoint :: Type -> Type -> TcMonad ()
disjoint TopT _ = return ()
disjoint _ TopT = return ()
disjoint t@(TVar x) t'@(TVar y) = do
  let l = do a <- lookupTyVar x
             a <: t'
             return ()
  let r = do a <- lookupTyVar y
             a <: t
             return ()
  mplus l r
disjoint (TVar x) b = do
  a <- lookupTyVar x
  a <: b
  return ()
disjoint b (TVar x) = do
  a <- lookupTyVar x
  a <: b
  return ()
disjoint (DForall t) (DForall t') = do
  ((x, Embed a1), b) <- unbind t
  ((y, Embed a2), c) <- unbind t'
  let c' = subst y (TVar x) c
  extendCtx (TyDef x (And a1 a2) Nothing) $ disjoint b c'
disjoint (DForall _) _ = return ()
disjoint _ (DForall _) = return ()
disjoint (SRecT l a) (SRecT l' b) =
  if l == l'
    then disjoint a b
    else return ()
disjoint (SRecT _ _) _ = return ()
disjoint _ (SRecT _ _) = return ()
disjoint (Arr _ a2) (Arr _ b2) =
  disjoint a2 b2
disjoint (Arr _ _) _ = return ()
disjoint _ (Arr _ _) = return ()
disjoint (And a1 a2) b = do
  disjoint a1 b
  disjoint a2 b
disjoint a (And b1 b2) = do
  disjoint a b1
  disjoint a b2
disjoint IntT BoolT = return ()
disjoint BoolT IntT = return ()
disjoint a b =
  throwStrErr $ "Types are not disjoint: " ++ pprint a ++ " and " ++ pprint b


transTyp :: Fresh m => Type -> m T.Type
transTyp IntT = return T.IntT
transTyp BoolT = return T.BoolT
transTyp (Arr t1 t2) = do
  t1' <- transTyp t1
  t2' <- transTyp t2
  return (T.Arr t1' t2')
transTyp (And t1 t2) = do
  t1' <- transTyp t1
  t2' <- transTyp t2
  return (T.Prod t1' t2')
transTyp (DForall t) = do
  ((x, _), body) <- unbind t
  b <- transTyp body
  return (T.Forall (bind (translate x) b))
transTyp (SRecT _ t) = transTyp t
transTyp TopT = return T.UnitT
transTyp (TVar x) = return . T.TVar . translate $ x

