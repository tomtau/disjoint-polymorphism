{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Target.Syntax where

import qualified Data.Text                        as T
import           Data.Typeable                    (Typeable)
import           GHC.Generics                     (Generic)
import           Text.PrettyPrint.ANSI.Leijen     (Doc, colon, dot, parens,
                                                   text, (<+>), (<>))
import           Unbound.Generics.LocallyNameless


type TmName = Name Expr


data Operation = Mul
               | Sub
               | Add
               deriving (Show, Generic, Typeable)


data Expr = Anno Expr Type
          | Var TmName
          | App Expr Expr
          | Lam (Bind TmName Expr)
          | Pair Expr Expr
          | Project Expr Int
          | IntV Int
          | BoolV Bool
          | Unit
          | PrimOp Operation Expr Expr
  deriving (Show, Generic, Typeable)


data Type = IntT
          | BoolT
          | UnitT
          | Arr Type Type
          | Product Type Type
  deriving (Show, Generic, Typeable)


addExpr :: Expr -> Expr -> Expr
addExpr = PrimOp Add

subExpr :: Expr -> Expr -> Expr
subExpr = PrimOp Sub

multExpr :: Expr -> Expr -> Expr
multExpr = PrimOp Mul


instance Alpha Type
instance Alpha Operation
instance Alpha Expr


instance Subst Expr Type
instance Subst Expr Operation
instance Subst Expr Expr where
  isvar (Var v) = Just (SubstName v)
  isvar _ = Nothing


evar :: String -> Expr
evar = Var . s2n

ebindt :: (String, Type) -> Expr -> Bind (TmName, Embed Type) Expr
ebindt (n, e1) = bind (s2n n, embed e1)

ebind :: String -> Expr -> Bind TmName Expr
ebind n = bind (s2n n)

elam :: String -> Expr -> Expr
elam x e = Lam (ebind x e)

eapp :: Expr -> Expr -> Expr
eapp = App


class Pretty p where
  ppr :: (Applicative m, LFresh m) => p -> m Doc


instance Pretty Type where
  ppr (Arr t1 t2) =
    do t1' <- ppr t1
       t2' <- ppr t2
       return $ parens (t1' <+> text "->" <+> t2')

  ppr IntT = return $ text "int"

  ppr BoolT = return $ text "bool"

  ppr (Product t1 t2) =
    do t1' <- ppr t1
       t2' <- ppr t2
       return $ parens (t1' <+> text "&" <+> t2')

  ppr UnitT = return $ text "()"


instance Pretty Expr where
  ppr (Anno e t) =
    do e' <- ppr e
       t' <- ppr t
       return $ e' <+> colon <+> t'

  ppr (Var x) = return . text . show $ x

  ppr (App f a) =
    do f' <- ppr f
       a' <- ppr a
       return $ parens (f' <+> a')

  ppr (Lam bnd) =
    lunbind bnd $
    \(x, b) ->
      do b' <- ppr b
         return (parens $ text "\\" <> (text . show $ x) <+> dot <+> b')

  ppr (IntV n) = return . text . show $ n

  ppr (BoolV b) = return . text . show $ b

  ppr (PrimOp op e1 e2) =
    do e1' <- ppr e1
       e2' <- ppr e2
       op' <- ppr op
       return $ parens (e1' <+> op' <+> e2')

  ppr (Pair e1 e2) =
    do e1' <- ppr e1
       e2' <- ppr e2
       return $ parens (e1' <+> ",," <+> e2')

  ppr (Project e i) =
    do e' <- ppr e
       return $ e' <> dot <> text (show i)

  ppr Unit = return $ text "()"


instance Pretty Operation where
  ppr Add = return . text $ "+"
  ppr Mul = return . text $ "*"
  ppr Sub = return . text $ "-"


showExpr :: Expr -> String
showExpr = show . runLFreshM . ppr
