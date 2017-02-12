
module Source.Typing where

import           Common
import           Control.Monad
import           Env
import           PrettyPrint
import           Source.Subtyping
import           Source.Syntax
import qualified Target.Syntax as T
import           Unbound.LocallyNameless




------------------------
-- Γ ⊢ e ⇒ A ~> E
------------------------

infer :: Expr -> TMonad (Type, T.Expr)

{-

Γ ⊢ ⊤ ⇒ ⊤  ~> ()

-}
infer Top = return (TopT, T.Unit)

{-

Γ ⊢ i ⇒ Int ~> i

-}
infer (IntV n) = return (IntT, T.IntV n)

{-

Γ ⊢ b ⇒ bool ~> b

-}
infer (BoolV b) = return (BoolT, T.BoolV b)

{-

   x:A ∈ Γ
---------------
Γ ⊢ x ⇒ A ~> x

-}
infer (Var x) = do
  t <- lookupTy (Trm x)
  return (t, T.Var (translate x))  -- Change the sort of a name

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
Γ ⊢ e1 ⇐ A1        ~> E2
----------------------------
Γ ⊢ e1 e2 ⇒ A2     ~> E1 E2

-}
infer (App e1 e2) = do
  (arr, e1') <- infer e1
  case arr of
    Arr a1 a2 -> do
      e2' <- check e2 a1
      return (a2, T.App e1' e2')
    _ -> throwStrErr $ pprint arr ++ " is not an arrow type"

{-

Γ ⊢ e ⇒ ∀(α ∗ B). C  ~> E
Γ ⊢ A
Γ ⊢ A ∗ B
-------------------------------
Γ ⊢ e A ⇒ [α := A] C  ~> E |A|

-}
infer (TApp e a) = do
  (t, e') <- infer e
  wf a
  case t of
    DForall t' -> do
      ((x, Embed b), c) <- unbind t'
      disjoint a b
      a' <- transTyp a
      return (subst x a c, T.TApp e' a')
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
  return (And a b, T.Pair e1' e2')

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
  (t, e) <- infer e
  case t of
    SRecT l' a ->
      if l == l'
        then return (a, e)
        else throwStrErr $ "labels not equal" ++ l ++ " and " ++ l'
    _ -> throwStrErr $ pprint e ++ " is not a record"

{-

Γ ⊢ A
Γ , a * A ⊢ e ⇒ B ~> E
a fresh
---------------------------------
Γ ⊢ Λ(α∗A).e ⇒ ∀(α∗A).B ~> Λα.E

-}
infer (DLam t) = do
  ((x, Embed a), e) <- unbind t
  wf a
  (b, e') <- extendCtx (Typ x, a) $ infer e
  return (DForall (bind (x, embed a) b), T.BLam (bind (translate x) e'))

infer (PrimOp op e1 e2) =
  case op of
    Arith _ -> do
      e1' <- check e1 IntT
      e2' <- check e2 IntT
      return (IntT, T.PrimOp op e1' e2')
    Logical _ -> do
      e1' <- check e1 IntT
      e2' <- check e2 IntT
      return (BoolT, T.PrimOp op e1' e2')

infer (If e1 e2 e3) = do
  e1' <- check e1 BoolT
  (t2, e2') <- infer e2
  (t3, e3') <- infer e3
  if aeq t2 t3
    then return (t2, T.If e1' e2' e3')
    else throwStrErr $ pprint t2 ++ " and " ++ pprint t3 ++ " must be the same type"

infer a = throwStrErr $ "Infer not implemented: " ++ pprint a

------------------------
-- Γ ⊢ e ⇐ A ~> E
------------------------

check :: Expr -> Type -> TMonad T.Expr

{-

Γ ⊢ A
Γ , x:A ⊢ e ⇐ B ~> E
---------------------------
Γ ⊢ λx. e ⇐ A → B ~> λx. E

-}
check (Lam l) (Arr a b) = do
  (x, e) <- unbind l
  wf a
  e' <- extendCtx (Trm x, a) $ check e b
  return (T.Lam (bind (translate x) e'))

{-
Γ ⊢ e ⇒ A ~> E
A <: B ~> Esub
Γ ⊢ B
-------------------
Γ ⊢ e ⇐ B ~> Esub E

-}
check e b = do
  (a, e) <- infer e
  c <- a <: b
  wf b
  return (T.App c e)



wf :: Type -> TMonad ()
wf IntT = return ()
wf BoolT = return ()
wf (Arr a b) = wf a >> wf b
wf (And a b) = wf a >> wf b >> disjoint a b
wf (TVar x) = lookupTy (Typ x) >> return ()
wf (DForall t) = do
  ((x, Embed a),b) <- unbind t
  wf a
  extendCtx (Typ x, a) $ wf b
wf (SRecT l t) = wf t
wf TopT = return ()

disjoint :: Type -> Type -> TMonad ()
disjoint TopT _ = return ()
disjoint _ TopT = return ()
disjoint t@(TVar x) t'@(TVar y) = do
  let l = do a <- lookupTy (Typ x)
             a <: t'
             return ()
  let r = do a <- lookupTy (Typ y)
             a <: t
             return ()
  mplus l r
disjoint (TVar x) b = do
  a <- lookupTy (Typ x)
  a <: b
  return ()
disjoint b (TVar x) = do
  a <- lookupTy (Typ x)
  a <: b
  return ()
disjoint (DForall t) (DForall t') = do
  ((x, Embed a1), b) <- unbind t
  ((y, Embed a2), c) <- unbind t'
  let c' = subst y (TVar x) c
  extendCtx (Typ x, And a1 a2) $ disjoint b c'
disjoint (DForall _) _ = return ()
disjoint (SRecT l a) (SRecT l' b) =
  if l == l'
    then disjoint a b
    else return ()
disjoint (SRecT _ _) _ = return ()
disjoint (Arr a1 a2) (Arr b1 b2) =
  disjoint a2 b2
disjoint (Arr _ _) _ = return ()
disjoint (And a1 a2) b = do
  disjoint a1 b
  disjoint a2 b
disjoint a (And b1 b2) = do
  disjoint a b1
  disjoint a b2
disjoint IntT BoolT = return ()
disjoint BoolT IntT = return ()
disjoint a b = throwStrErr $ "Types are not disjoint: " ++ pprint a ++ " and " ++ pprint b


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
  ((x, p), body) <- unbind t
  b <- transTyp body
  return (T.Forall (bind (translate x) b))
transTyp (SRecT l t) = transTyp t
transTyp TopT = return T.UnitT
transTyp (TVar x) = return . T.TVar . translate $ x


----------
-- Test
----------

-- (/\ A. /\ (B * A). A&B -> A
fst_ex :: Type
fst_ex = (DForall (bind (s2n "A", embed TopT) (DForall (bind (s2n "B", embed (tvar "A")) (Arr (And (tvar "A") (tvar "B")) (tvar "A"))))))

-- (\fst. fst int bool (1,,true))
test1 :: Expr
test1 =
  DLam
    (bind
       (s2n "A", embed TopT)
       (DLam
          (bind
             (s2n "B", embed (tvar "A"))
             (Anno
                (Lam (bind (s2n "x") (evar "x")))
                (Arr (And (tvar "A") (tvar "B")) (tvar "A"))))))
