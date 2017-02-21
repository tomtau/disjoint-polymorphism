{-# LANGUAGE FlexibleContexts #-}

module Source.Subtyping where

import           Control.Monad.Except
import           Environment
import           PrettyPrint
import           Source.Syntax
import qualified Target.Syntax as T
import           Unbound.LocallyNameless
import           Text.PrettyPrint.ANSI.Leijen hiding (Pretty)

----------------------------
-- A <: B ~> E

-- note: target is untyped
----------------------------

(<:)
  :: (Fresh m, MonadError Doc m, MonadPlus m)
  => Type -> Type -> m T.UExpr

{-

A <: ⊤  ~> λx.()

-}
(<:) _ TopT = return (T.elam "x" T.UUnit)

{-

A1 <: A2 ~> E1       A1 <: A3 ~> E2
--------------------------------------
A1 <: A2&A3   ~>  λx. (E1 x, E2 x)

-}
(<:) a1 (And a2 a3) = do
  e1 <- a1 <: a2
  e2 <- a1 <: a3
  let co = T.elam "x" (T.UPair (T.eapp e1 (T.evar "x")) (T.eapp e2 (T.evar "x")))
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
(<:) (And a1 a2) a3
  | ordinary a3 =
    let left = do
          e <- a1 <: a3
          let c = T.eapp e (T.UP1 (T.evar "x"))
          b <- coerce a3 c
          return (T.elam "x" b)
        right = do
          e <- a2 <: a3
          let c = T.eapp e (T.UP2 (T.evar "x"))
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
    then throwError $
         text "labels not equal:" <+> text l1 <+> text "and" <+> text l2
    else return e


{-

a <: a ~> λx.x

-}
(<:) (TVar a) (TVar b) = do
  if a /= b
    then throwError $
         text "variables not equal:" <+>
         text (show a) <+> text "and" <+> text (show b)
    else return (T.elam "x" (T.evar "x"))

{-

B1 <: A1 ~> E1         A2 <: B2 ~> E2
-------------------------------------------------
A1 -> A2 <: B1 -> B2   ~> λf . λ x . E2 (f (E1 x))

-}
(<:) (Arr a1 a2) (Arr b1 b2) = do
  e1 <- b1 <: a1
  e2 <- a2 <: b2
  let body = T.eapp e2 (T.eapp (T.evar "f") (T.eapp e1 (T.evar "x")))
  return $ T.elam "f" (T.elam "x" body)

{-

B1 <: B2 ~> E1    A2 <: A1 ~> E2
------------------------------------------------
∀(a*A1).B1 <: ∀(a*A2).B2   ~> λf. Λa . E1 (f a)

-}
(<:) (DForall t1) (DForall t2) = do
  t <- unbind2 t1 t2
  case t of
    Just ((_, Embed a1), b1, (_, Embed a2), b2) -> do
      a2 <: a1
      b1 <: b2
    Nothing -> throwError . text $ "Patterns have different binding variables"

(<:) a b = throwError $ text "Invalid subtyping:" <+> pprint a <+> (text "and") <+> pprint b


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

coerce
  :: (Fresh m, MonadError Doc m)
  => Type -> T.UExpr -> m T.UExpr
coerce a c = do
  isTopLike <- topLike a
  if isTopLike
    then coerce' a
    else return c
  where
    coerce'
      :: (Fresh m, MonadError Doc m)
      => Type -> m T.UExpr
    coerce' TopT = return T.UUnit
    coerce' (Arr _ a2) = do
      a2' <- coerce' a2
      return (T.elam "x" a2')
    coerce' (And a1 a2) = do
      a1' <- coerce' a1
      a2' <- coerce' a2
      return $ T.UPair a1' a2'
    coerce' (SRecT _ a) = coerce' a
    coerce' (DForall t) = do
      ((_,  _), a) <- unbind t
      coerce' a
    coerce' t = throwError $ text "Cannot coerce" <+> pprint t

------------
-- ⌉A⌈
------------

topLike :: Fresh m => Type -> m Bool
topLike TopT = return True
topLike (And a b) = do
  a' <- topLike a
  b' <- topLike b
  return (a' && b')
topLike (Arr _ b) = topLike b
topLike (SRecT _ a) = topLike a
topLike (DForall t) = do
  ((_, _), a) <- unbind t
  topLike a
topLike _ = return False
