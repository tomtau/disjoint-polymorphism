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


type TmName = Name Expr


data Expr = Anno Expr Type
          | Var TmName
          | App Expr Expr
          | Lam (Bind TmName Expr)
          | Let (Bind (TmName, Embed Type) (Expr, Expr)) -- recursive let
          | DLam (Bind (TyName, Embed Type) Expr)
          | TApp Expr Type
          | DRec Label Expr
          | Acc Expr Label
          | Merge Expr Expr
          | IntV Int
          | BoolV Bool
          | PrimOp Operation Expr Expr
          | If Expr Expr Expr
          -- | FixP (Bind TmName Expr)
          | Top
  deriving Show

type TyName = Name Type
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


data TcName
  = Trm TmName
  | Typ TyName
  deriving Eq

instance Show TcName where
  show (Trm x) = show x
  show (Typ x) = show x

type TMonad = TcMonad TcName Type
