{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Target.Syntax where

import Common
import Unbound.LocallyNameless

type TmName = Name Expr



-------------
-- Sysyem F
-------------

data Expr = Var TmName
          | App Expr Expr
          | Lam (Bind TmName Expr)
          | BLam (Bind TyName Expr)
          | Let (Bind TmName (Expr, Expr)) -- recursive let
          | TApp Expr Type
          | Pair Expr Expr
          | Proj1 Expr
          | Proj2 Expr
          | IntV Int
          | BoolV Bool
          | Unit
          | PrimOp Operation Expr Expr
          | If Expr Expr Expr
          -- | FixP (Bind TmName Expr)
  deriving Show


type TyName = Name Type

data Type = TVar TyName
          | IntT
          | BoolT
          | UnitT
          | Arr Type Type
          | Forall (Bind TyName Type)
          | Prod Type Type
    deriving Show


$(derive [''Expr, ''Type])

instance Alpha Type
instance Alpha Expr

instance Subst Expr Type
instance Subst Expr ArithOp
instance Subst Expr LogicalOp
instance Subst Expr Operation
instance Subst Expr Expr where
  isvar (Var v) = Just (SubstName v)
  isvar _ = Nothing
instance Subst Type Expr
instance Subst Type Operation
instance Subst Type LogicalOp
instance Subst Type ArithOp
instance Subst Type Type where
  isvar (TVar v) = Just (SubstName v)
  isvar _ = Nothing


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
           | UIntV Int
           | UBoolV Bool
           | UUnit
           | UPrimOp Operation UExpr UExpr
           | UIf UExpr UExpr UExpr
           deriving Show

instance Alpha UExpr

$(derive [''UExpr])

instance Subst UExpr ArithOp
instance Subst UExpr LogicalOp
instance Subst UExpr Operation
instance Subst UExpr UExpr where
  isvar (UVar v) = Just (SubstName v)
  isvar _ = Nothing

evar :: String -> UExpr
evar = UVar . s2n

tvar :: String -> Type
tvar = TVar . s2n

ebind :: String -> UExpr -> Bind UName UExpr
ebind n = bind (s2n n)

elam :: String -> UExpr -> UExpr
elam b e = ULam (ebind b e)

eapp :: UExpr -> UExpr -> UExpr
eapp = UApp
