
module Source.Subtyping where

import           Common
import           Control.Monad.Except
import           Data.Text
import           Env
import           PrettyPrint
import           Source.Syntax
import qualified Target.Syntax as T
import           Unbound.LocallyNameless

----------------
-- A <: B ~> E
----------------

(<:) :: Type -> Type -> TMonad T.Expr

{-

A <: ⊤  ~> λx.()

-}
(<:) _ TopT = return (T.elam "x" T.Unit)

{-

A1 <: A2 ~> E1       A1 <: A3 ~> E2
--------------------------------------
A1 <: A2&A3   ~>  λx. (E1 x, E2 x)

-}
(<:) a1 (And a2 a3) = do
  e1 <- a1 <: a2
  e2 <- a1 <: a3
  let co = T.elam "x" (T.Pair (T.App e1 (T.evar "x")) (T.App e2 (T.evar "x")))
  return co

{-

Int <: Int ~> λx. x

-}
(<:) IntT IntT = return (T.elam "x" (T.evar "x"))
{-

Bool <: Bool ~> λx. x

-}
(<:) BoolT BoolT = return (T.elam "x" (T.evar "x"))

{-

A1 <: A3 ~> E     A3 ordinary
-------------------------------
A1&A2 <: A3 ~> λx.[[A3]](E (proj1 x))

A2 <: A3 ~> E      A3 ordinary
--------------------------------
A1&A2 <: A3 ~> λx . [[A3]](E (proj2 x))


-}
(<:) (And a1 a2) a3 =
  let left = do e <- a1 <: a3
                let c = T.App e (T.Proj1 (T.evar "x"))
                b <- coerce a3 c
                return (T.elam "x" b)
      right = do e <- a2 <: a3
                 let c = T.App e (T.Proj2 (T.evar "x"))
                 b <- coerce a3 c
                 return (T.elam "x" b)

  in mplus left right

{-

A <: B ~> E
-----------------
{l:A} <: {l : B}

-}
(<:) (SRecT l1 a) (SRecT l2 b) = do
  e <- a <: b
  if (l1 /= l2)
    then throwStrErr $ "labels not equal: " ++ l1 ++ " and " ++ l2
    else return e


{-

a <: a ~> λx.x

-}
(<:) (TVar a) (TVar b) = do
  if a /= b
    then throwStrErr $ "variables not equal: " ++ show a ++ " and " ++ show b
    else return (T.elam "x" (T.evar "x"))

{-

B1 <: A1 ~> E1         A2 <: B2 ~> E2
-------------------------------------------------
A1 -> A2 <: B1 -> B2   ~> λf . λ x . E2 (f (E1 x))

-}
(<:) (Arr a1 a2) (Arr b1 b2) = do
  e1 <- b1 <: a1
  e2 <- a2 <: b2
  let body = T.App e2 (T.App (T.evar "f") (T.App e1 (T.evar "x")))
  return $ T.elam "f" (T.elam "x" body)

{-

B1 <: B2 ~> E1    A2 <: A1 ~> E2
------------------------------------------------
∀(a*A1).B1 <: ∀(a*A2).B2   ~> λf. Λa . E1 (f a)

-}
(<:) (DForall t1) (DForall t2) = do
  ((a, Embed a1) , b1) <- unbind t1
  ((a', Embed a2) , b2) <- unbind t2
  let b2' = subst a' (TVar a) b2 -- FIXME: Overkill?
  e1 <- b1 <: b2'
  e2 <- a2 <: a1
  return $ T.elam "f" (T.blam "a" (T.App e1 (T.TApp (T.evar "f") (T.tvar "a"))))

(<:) a b = throwStrErr $ "Invalid subtyping: " ++ pprint a ++ " and " ++ pprint b


--------------
-- A ordinary
--------------

ordinary :: Type -> Bool
ordinary IntT = True
ordinary BoolT = True
ordinary (Arr _ _) = True
ordinary (TVar _) = True
ordinary (DForall _) = True
ordinary (SRecT _ _) = True
ordinary _ = False


---------------
-- [[A]]C = T
---------------

coerce :: Fresh m => Type -> T.Expr -> m T.Expr
coerce a c = do
  isTopLike <- topLike a
  if isTopLike
    then coerce' a
    else return c
  where
    coerce' :: Fresh m => Type -> m T.Expr
    coerce' TopT = return T.Unit
    coerce' (Arr a1 a2) = do
      a2' <- coerce' a2
      return (T.elam "x" a2')
    coerce' (And a1 a2) = do
      a1' <- coerce' a1
      a2' <- coerce' a2
      return $ T.Pair a1' a2'
    coerce' (SRecT _ a) = coerce' a
    coerce' (DForall t) = do
      ((_, _) , a) <- unbind t
      a' <- coerce' a
      return $ T.blam "b" a'

------------
-- ⌉A⌈
------------

topLike :: Fresh m => Type -> m Bool
topLike TopT = return True
topLike (And a b) = do
  a' <- topLike a
  b' <- topLike b
  return (a' && b')
topLike (Arr a b) = topLike b
topLike (SRecT _ a) = topLike a
topLike (DForall t) = do
  ((_, _), a) <- unbind t
  topLike a
topLike _ = return False
