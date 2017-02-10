
module Typing where

import Source.Syntax
import Common
import qualified Target.Syntax as T
import Env
import PrettyPrint
import Unbound.LocallyNameless
import Source.Subtyping




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
  return (t, T.evar $ show x)

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


check :: Expr -> Type -> TMonad T.Expr
check = undefined


wf :: Type -> TMonad ()
wf IntT = return ()
wf BoolT = return ()
wf (Arr a b) = wf a >> wf b
wf (And a b) = wf a >> wf b >> disjoint a b
wf (TVar x) = lookupTy (Typ x) >> return ()
wf (DForall t) = do
  ((x, a),b) <- unbind t
  wf a
  extendCtx (Typ x, a) $ wf b
wf (SRecT l t) = wf t
wf TopT = return ()

disjoint :: Type -> Type -> TMonad ()
disjoint TopT _ = return ()
disjoint _ TopT = return ()
disjoint (TVar x) b = do
  a <- lookupTy (Typ x)
  a <: b
  return ()
disjoint b (TVar x) = do
  a <- lookupTy (Typ x)
  a <: b
  return ()
disjoint (DForall t) (DForall t') = do
  ((x, a1), b) <- unbind t
  ((y, a2), c) <- unbind t'
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
disjoint a b = throwStrErr $ "Two types are not disjoint: " ++ pprint a ++ " and " ++ pprint b
