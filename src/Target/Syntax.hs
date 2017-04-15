{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Target.Syntax where

import Common
import Unbound.LocallyNameless


---------------------------
-- Untyped lambda calculus
---------------------------

type UName = Name UExpr

data UExpr = UVar UName
           | UApp UExpr UExpr
           | ULam (Bind UName UExpr)
           | ULet (Bind UName (UExpr, UExpr))  -- recursive let
           | UPair UExpr UExpr
           | UP1 UExpr
           | UP2 UExpr
           | ULitV Double
           | UBoolV Bool
           | UStrV String
           | UUnit
           | UPrimOp Operation UExpr UExpr
           | UIf UExpr UExpr UExpr
           | UToString UExpr
           | USqrt UExpr
           | Bot
           deriving Show

instance Alpha UExpr

$(derive [''UExpr])

instance Subst UExpr ArithOp
instance Subst UExpr LogicalOp
instance Subst UExpr Operation
instance Subst UExpr CompOp
instance Subst UExpr UExpr where
  isvar (UVar v) = Just (SubstName v)
  isvar _ = Nothing

evar :: String -> UExpr
evar = UVar . s2n

ebind :: String -> UExpr -> Bind UName UExpr
ebind n = bind (s2n n)

elam :: String -> UExpr -> UExpr
elam b e = ULam (ebind b e)

eapp :: UExpr -> UExpr -> UExpr
eapp = UApp
