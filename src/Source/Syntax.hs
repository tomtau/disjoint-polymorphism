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


type TmName = Name Expr


data Expr = Anno Expr Type
          | Var TmName
          | App Expr Expr
          | Lam (Bind TmName Expr)
          | DLam (Bind (TyName, Embed Type) Expr)
          | TApp Expr Type
          | DRec Label Expr
          | Acc Expr Label
          | Merge Expr Expr
          | IntV Int
          | BoolV Bool
          | PrimOp Operation Expr Expr
          | If Expr Expr Expr
          | FixP (Bind TmName Expr)
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

evar :: String -> Expr
evar = Var . s2n

tvar :: String -> Type
tvar = TVar . s2n

ebind :: String -> Expr -> Bind TmName Expr
ebind n = bind (s2n n)

elam :: String -> Expr -> Expr
elam b e = Lam (ebind b e)

eapp :: Expr -> Expr -> Expr
eapp = App

etapp :: Expr -> Type -> Expr
etapp = TApp
