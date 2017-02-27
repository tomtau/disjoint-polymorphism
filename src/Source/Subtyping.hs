{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}

module Source.Subtyping
  ( subtype
  ) where

import           Control.Monad.Except
import           Environment
import           PrettyPrint
import           Source.Syntax
import qualified Target.Syntax as T
import           Text.PrettyPrint.ANSI.Leijen hiding (Pretty)
import           Unbound.LocallyNameless
import Source.Desugar


subtype :: Ctx -> Type -> Type -> Either Doc T.UExpr
subtype d s t =
  runExcept $ runFreshMT (subtypeS d (expandType d s) (expandType d t))


----------------------------
-- A <: B ~> E

-- note: target is untyped
----------------------------

subtypeS :: Ctx -> Type -> Type -> (FreshMT (Except Doc)) T.UExpr

{-

A <: ⊤  ~> λx.()

-}
subtypeS _ _ TopT = return (T.elam "x" T.UUnit)

{-

A1 <: A2 ~> E1       A1 <: A3 ~> E2
--------------------------------------
A1 <: A2&A3   ~>  λx. (E1 x, E2 x)

-}
subtypeS d a1 (And a2 a3) = do
  e1 <- subtypeS d a1 a2
  e2 <- subtypeS d a1 a3
  let co = T.elam "x" (T.UPair (T.eapp e1 (T.evar "x")) (T.eapp e2 (T.evar "x")))
  return co

subtypeS _ IntT IntT = return (T.elam "x" (T.evar "x"))

subtypeS _ BoolT BoolT = return (T.elam "x" (T.evar "x"))

subtypeS _ StringT StringT = return (T.elam "x" (T.evar "x"))

{-

A1 <: A3 ~> E     A3 ordinary
-------------------------------
A1&A2 <: A3 ~> λx.[[A3]](E (proj1 x))

-}
subtypeS d (And a1 _) a3
  | ordinary a3
  , Right e <- subtype d a1 a3 = do
    let c = T.eapp e (T.UP1 (T.evar "x"))
    b <- coerce a3 c
    return (T.elam "x" b)

{-

A2 <: A3 ~> E      A3 ordinary
--------------------------------
A1&A2 <: A3 ~> λx . [[A3]](E (proj2 x))


-}
subtypeS d (And _ a2) a3
  | ordinary a3
  , Right e <- subtype d a2 a3 = do
    let c = T.eapp e (T.UP2 (T.evar "x"))
    b <- coerce a3 c
    return (T.elam "x" b)

{-

A <: B ~> E
-----------------
{l:A} <: {l : B}

-}
subtypeS d (SRecT l1 a) (SRecT l2 b) = do
  e <- subtypeS d a b
  if (l1 /= l2)
    then throwError $
         text "labels not equal:" <+> text l1 <+> text "and" <+> text l2
    else return e


{-

a <: a ~> λx.x

-}
subtypeS _ (TVar a) (TVar b) = do
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
subtypeS d (Arr a1 a2) (Arr b1 b2) = do
  e1 <- subtypeS d b1 a1
  e2 <- subtypeS d a2 b2
  let body = T.eapp e2 (T.eapp (T.evar "f") (T.eapp e1 (T.evar "x")))
  return $ T.elam "f" (T.elam "x" body)

{-

B1 <: B2 ~> E1    A2 <: A1 ~> E2
------------------------------------------------
∀(a*A1).B1 <: ∀(a*A2).B2   ~> λf. Λa . E1 (f a)

-}
subtypeS d (DForall t1) (DForall t2) = do
  t <- unbind2 t1 t2
  case t of
    Just ((_, Embed a1), b1, (_, Embed a2), b2) -> do
      subtypeS d a2 a1
      subtypeS d b1 b2
    Nothing -> throwError . text $ "Patterns have different binding variables"

subtypeS _ a b =
  throwError $
  text "Invalid subtyping:" <+>
  squotes (pprint a) <+> (text "and") <+> squotes (pprint b)


--------------
-- A ordinary
--------------

ordinary :: Type -> Bool
ordinary IntT = True
ordinary BoolT = True
ordinary StringT = True
ordinary (Arr _ _) = True
ordinary (TVar _) = True
ordinary (DForall _) = True
ordinary (SRecT _ _) = True
ordinary _ = False


---------------
-- [[A]]C = T
---------------

coerce
  :: Fresh m
  => Type -> T.UExpr -> m T.UExpr
coerce a c = do
  isTopLike <- topLike a
  if isTopLike
    then coerce' a
    else return c
  where
    coerce'
      :: (Fresh m)
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
      ((_, _), a) <- unbind t
      coerce' a
    coerce' _ = error "Impossible happened in coercing!"

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
