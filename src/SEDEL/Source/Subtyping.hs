{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module SEDEL.Source.Subtyping
  ( subtype
  ) where


import           Panic
import           Prelude (unzip)
import           Protolude hiding (Type)
import           Text.PrettyPrint.ANSI.Leijen hiding (Pretty)
import           Unbound.LocallyNameless

import           SEDEL.Environment
import           SEDEL.PrettyPrint
import           SEDEL.Source.Desugar
import           SEDEL.Source.Syntax
import qualified SEDEL.Target.Syntax as T

----------------------------
-- A <: B ~> E
-- note: target is untyped
----------------------------
-- | Subtyping (<:) is defined only between types of kind *.
-- WARN: They must be expanded first
subtype :: Ctx -> Type -> Type -> Either Doc T.UExpr
subtype ctx st tt = runExcept $ runFreshMT go
  where
    go :: (FreshMT (Except Doc)) T.UExpr
    go = do
      let a = expandType ctx st
      let b = expandType ctx tt
      subtypeS a b
    subtypeS :: Type -> Type -> (FreshMT (Except Doc)) T.UExpr
    {-

    A <: ⊤  ~> λx.()

    -}
    subtypeS _ TopT = return (T.elam "x" T.UUnit)
    {-

    A1 <: A2 ~> E1       A1 <: A3 ~> E2
    --------------------------------------
    A1 <: A2&A3   ~>  λx. (E1 x, E2 x)

    -}
    subtypeS a1 (And a2 a3) = do
      e1 <- subtypeS a1 a2
      e2 <- subtypeS a1 a3
      let co =
            T.elam
              "x"
              (T.UPair (T.eapp e1 (T.evar "x")) (T.eapp e2 (T.evar "x")))
      return co
    subtypeS NumT NumT = return (T.elam "x" (T.evar "x"))
    subtypeS BoolT BoolT = return (T.elam "x" (T.evar "x"))
    subtypeS StringT StringT = return (T.elam "x" (T.evar "x"))
    {-

    A1 <: A3 ~> E     A3 ordinary
    -------------------------------
    A1&A2 <: A3 ~> λx.[[A3]](E (proj1 x))

    -}
    subtypeS (And a1 _) a3
      | ordinary a3
      , Right e <- subtype ctx a1 a3 = do
        let c = T.eapp e (T.UP1 (T.evar "x"))
        b <- coerce a3 c
        return (T.elam "x" b)
    {-

    A2 <: A3 ~> E      A3 ordinary
    --------------------------------
    A1&A2 <: A3 ~> λx . [[A3]](E (proj2 x))

    -}
    subtypeS (And _ a2) a3
      | ordinary a3
      , Right e <- subtype ctx a2 a3 = do
        let c = T.eapp e (T.UP2 (T.evar "x"))
        b <- coerce a3 c
        return (T.elam "x" b)
    {-

    A <: B ~> E
    -----------------
    {l:A} <: {l : B}

    -}
    subtypeS (SRecT l1 a) (SRecT l2 b) = do
      e <- subtypeS a b
      if (l1 /= l2)
        then throwError $
             text "labels not equal:" <+> text l1 <+> text "and" <+> text l2
        else return e
    {-

    a <: a ~> λx.x

    -}
    subtypeS (TVar a) (TVar b) = do
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
    subtypeS (Arr a1 a2) (Arr b1 b2) = do
      e1 <- subtypeS b1 a1
      e2 <- subtypeS a2 b2
      let body = T.eapp e2 (T.eapp (T.evar "f") (T.eapp e1 (T.evar "x")))
      return $ T.elam "f" (T.elam "x" body)
    {-

    B1 <: B2 ~> E1    A2 <: A1 ~> E2
    ------------------------------------------------
    ∀(a*A1).B1 <: ∀(a*A2).B2   ~> E1

    -}
    subtypeS (DForall t1) (DForall t2) =
      unbind2 t1 t2 >>= \case
        Just ((_, Embed a1), b1, (_, Embed a2), b2) -> do
          subtypeS a2 a1
          subtypeS b1 b2
        Nothing ->
          throwError . text $ "Patterns have different binding variables"
    {-

    Intersections distribute over records

       A <: {l : forall D1 ... Dm . A1 -> .. C -> .. An -> A'} & {l : forall D1 ... Dm . B1 -> ... -> C -> Bn -> B'} ~> f
      ----------------------------------------------------------------
        A <: {l : forall D1 ... Dm . A1 & B1 -> ... -> C -> ... -> An & Bn -> A' & B'} ~>

        \s . \x1 ... y .. xn . ((proj1 (f s)) (proj1 x1) ... y ... (proj1 xn), (proj2 (f s)) (proj2 x1) ... y ... (proj2 xn))


     The view pattern below ensures that we match exactly the form: a -> ... -> a' & b'

    -}
    subtypeS a (SRecT l (lastForall -> (tyBinds, lastArr -> (t@(_:_), (And a' b'))))) = do
      f <- subtypeS a (And (SRecT l left) (SRecT l right))
      -- generate n fresh names [x1, ..., xn]
      let x = (s2n "x") :: T.UName
      xs <- sequenceA (replicate n (fresh x))
      let s = (s2n "s") :: T.UName
      -- first and second components in a pair
      let pair1 =
            foldl
              T.UApp
              (T.UP1 (T.UApp f (T.UVar s)))
              (zipWith ($) aprojs (map T.UVar xs))
      let pair2 =
            foldl
              T.UApp
              (T.UP2 (T.UApp f (T.UVar s)))
              (zipWith ($) bprojs (map T.UVar xs))
      -- Final expression
      let ebody = foldr (\xi e -> T.ULam (bind xi e)) (T.UPair pair1 pair2) xs
      return (T.ULam (bind s ebody))
      where
        (as, bs) = splitMerge2Arrow t
        (atyps, aprojs) = unzip as
        (btyps, bprojs) = unzip bs
        left = mkForall (mkArr a' atyps) tyBinds
        right = mkForall (mkArr b' btyps) tyBinds
        n = length as
    subtypeS a b =
      throwError $
      text "Invalid subtyping:" <+>
      squotes (pprint a) <+> (text "and") <+> squotes (pprint b)



{-

From

a -> b -> c -> d

to

([a,b,c], d)

-}
lastArr :: Type -> ([Type], Type)
lastArr (Arr a b) = first (a :) (lastArr b)
lastArr a = ([], a)

lastForall :: Type -> ([(TyName, Embed Type)], Type)
lastForall = runFreshM . go
  where
    go :: Fresh m => Type -> m ([(TyName, Embed Type)], Type)
    go (DForall b) = do
      (x, t) <- unbind b
      fmap (first (x :)) $ go t
    go a = return ([], a)


{-

transform

[A1 & B1, ..., C, ... -> An & Bn]

to

[A1, ..., C, ..., An]

and

[B1, ..., C, ..., Bn]

with projections

-}

splitMerge2Arrow :: [Type] -> ([(Type, T.UExpr -> T.UExpr)], [(Type, T.UExpr -> T.UExpr)])
splitMerge2Arrow [] = ([], [])
splitMerge2Arrow ((And a b):rest) =
  let (as, bs) = splitMerge2Arrow rest
  in ((a, T.UP1) : as, (b, T.UP2) : bs)
splitMerge2Arrow (a:rest) =
  let (as, bs) = splitMerge2Arrow rest
  in ((a, identity) : as, (a, identity) : bs)


--------------
-- A ordinary
--------------

ordinary :: Type -> Bool
ordinary NumT = True
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
    coerce' (SRecT _ t) = coerce' t
    coerce' (DForall t) = do
      ((_, _), t') <- unbind t
      coerce' t'
    coerce' _ = panic "Impossible happened in coercing!"

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
