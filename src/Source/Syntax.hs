{-# LANGUAGE MultiParamTypeClasses
           , TemplateHaskell
           , ScopedTypeVariables
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
#-}

module Source.Syntax where

import Common
import Unbound.LocallyNameless
import Env

-- Unbound library
type TmName = Name Expr
type TyName = Name Type

-- Expression
data Expr = Anno Expr Type
          | Var TmName
          | App Expr Expr
          | Lam (Bind TmName Expr)
          | Let (Bind (TmName, Embed Type, Embed Expr) Expr) -- recursive let
          | DLam (Bind (TyName, Embed Type) Expr)
          | TApp Expr Type
          | DRec Label Expr
          | Acc Expr Label
          | Merge Expr Expr
          | IntV Int
          | BoolV Bool
          | PrimOp Operation Expr Expr
          | If Expr Expr Expr
          | Top
  deriving Show

type Label = String
data Type = IntT
          | BoolT
          | Arr Type Type
          | And Type Type
          | TVar TyName
          | DForall (Bind (TyName, Embed Type) Type)
          | SRecT Label Type
          | TopT
  deriving Show


-- Declaration





-- Unbound library instances
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



-- Context holds term variables or type variables
data TcName
  = Trm TmName
  | Typ TyName
  deriving Eq

instance Show TcName where
  show (Trm x) = show x
  show (Typ x) = show x

type TMonad = TcMonad TcName Type



-- Utility for parsing

evar :: String -> Expr
evar = Var . s2n

tvar :: String -> Type
tvar = TVar . s2n

ebind :: String -> Expr -> Bind TmName Expr
ebind n = bind (s2n n)

elam :: String -> Expr -> Expr
elam b e = Lam (ebind b e)

dlam :: String -> Type -> Expr -> Expr
dlam s t b = DLam (bind (s2n s, embed t) b)

tforall :: String -> Type -> Type -> Type
tforall s t b = DForall (bind (s2n s, embed t) b)

eapp :: Expr -> Expr -> Expr
eapp = App

etapp :: Expr -> Type -> Expr
etapp = TApp

mkRecds :: [(Label, Expr)] -> Expr
mkRecds [(l, e)] = DRec l e
mkRecds ((l, e) : r) = Merge (DRec l e) (mkRecds r)

mkRecdsT :: [(Label, Type)] -> Type
mkRecdsT [(l, e)] = SRecT l e
mkRecdsT ((l, e) : r) = And (SRecT l e) (mkRecdsT r)

elet :: String -> Type -> Expr -> Expr -> Expr
elet s t e b = Let (bind (s2n s, embed t, embed e) b)
