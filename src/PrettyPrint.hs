{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint
  ( pprint
  , warn
  , info
  ) where

import           Common
import qualified Source.Syntax as S
import qualified Target.Syntax as T
import           Text.PrettyPrint.ANSI.Leijen hiding (Pretty)
import           Unbound.LocallyNameless


class Pretty p where
  ppr :: (Applicative m, LFresh m) => p -> m Doc


instance Pretty ArithOp where
  ppr Add = return $ text "+"
  ppr Mul = return $ text "*"
  ppr Sub = return $ text "-"
  ppr Div = return $ text "/"


instance Pretty LogicalOp where
  ppr Equ = return $ text "=="
  ppr Neq = return $ text "!="
  ppr Lt = return $ text "<"
  ppr Gt = return $ text ">"


instance Pretty Operation where
  ppr (Arith a) = ppr a
  ppr (Logical a) = ppr a
  ppr Append = return $ text "++"


instance Pretty S.Kind where
  ppr S.Star = return $ text "star"
  ppr (S.KArrow k1 k2) = do
    k1' <- ppr k1
    k2' <- ppr k2
    return $ k1' <+> text "->" <+> k2'

instance Pretty S.Type where
  ppr (S.Arr t1 t2) = do
    t1' <- ppr t1
    t2' <- ppr t2
    return $ parens (t1' <+> text "->" <+> t2')
  ppr S.IntT = return $ text "Int"
  ppr S.BoolT = return $ text "Bool"
  ppr S.StringT = return $ text "String"
  ppr (S.And t1 t2) = do
    t1' <- ppr t1
    t2' <- ppr t2
    return $ parens (t1' <+> text "&" <+> t2')
  ppr (S.TVar x) = return . text . name2String $ x
  ppr (S.DForall b) =
    lunbind b $ \((x, Embed a), t) -> do
      a' <- ppr a
      t' <- ppr t
      return
        (parens $
         text "∀" <> parens (text (name2String x) <> text "*" <> a') <+> dot <+> t')
  ppr (S.SRecT l t) = do
    t' <- ppr t
    return (braces $ (text l) <+> colon <+> t')
  ppr S.TopT = return $ text "T"
  ppr (S.OpAbs b) =
    lunbind b $ \(x, t) -> do
      t' <- ppr t
      return $ parens (text "Lam" <+> text (name2String x) <+> dot <+> t')
  ppr (S.OpApp a b) = do
    a' <- ppr a
    b' <- ppr b
    return $ parens (a' <+> brackets b')


instance Pretty S.Expr where
  ppr (S.Anno e t) = do
    e' <- ppr e
    t' <- ppr t
    return $ e' <+> colon <+> t'
  ppr (S.Var x) = return . text . name2String $ x
  ppr (S.App f a) = do
    f' <- ppr f
    a' <- ppr a
    return $ parens (f' <+> a')
  ppr (S.TApp f a) = do
    f' <- ppr f
    a' <- ppr a
    return $ parens (f' <> text "@" <> a')
  ppr (S.Lam bnd) =
    lunbind bnd $ \(x, b) -> do
      b' <- ppr b
      return (parens $ text "λ" <> text (name2String x) <+> dot <+> b')
  ppr (S.LamA bnd) =
    lunbind bnd $ \((x, Embed t), b) -> do
      b' <- ppr b
      t' <- ppr t
      return (parens $ text "λ" <> text (name2String x) <> colon <> t' <+> dot <+> b')
  ppr (S.DLam bnd) =
    lunbind bnd $ \((x, Embed t), b) -> do
      b' <- ppr b
      t' <- ppr t
      return
        (parens $
         text "Λ" <> parens (text (name2String x) <> text "*" <> t') <+> dot <+> b')
  ppr (S.IntV n) = return . text . show $ n
  ppr (S.BoolV True) = return (text "true")
  ppr (S.BoolV False) = return (text "false")
  ppr (S.StrV b) = return . text $ show b
  ppr (S.PrimOp op e1 e2) = do
    e1' <- ppr e1
    e2' <- ppr e2
    op' <- ppr op
    return $ parens (e1' <+> op' <+> e2')
  ppr (S.Merge e1 e2) = do
    e1' <- ppr e1
    e2' <- ppr e2
    return $ parens (e1' <+> ",," <+> e2')
  ppr (S.If p e1 e2) = do
    p' <- ppr p
    e1' <- ppr e1
    e2' <- ppr e2
    return $ text "if" <+> p' <+> text "then" <+> e1' <+> text "else" <+> e2'
  ppr (S.DRec l e) = do
    e' <- ppr e
    return $ braces (text l <+> text "=" <+> e')
  ppr (S.Acc e l) = do
    e' <- ppr e
    return $ e' <> dot <> text l
  ppr S.Top = return $ text "T"
  ppr (S.Let b) = do
    lunbind b $ \((x, Embed t), (e, body)) -> do
      e' <- ppr e
      t' <- ppr t
      b' <- ppr body
      return $
        text "let" <+>
        text (name2String x) <+> colon <+> t' <+> text "=" <+> e' <+> text "in" <+> b'


instance Pretty T.Type where
  ppr (T.Arr t1 t2) = do
    t1' <- ppr t1
    t2' <- ppr t2
    return $ parens (t1' <+> text "->" <+> t2')
  ppr (T.TVar x) = return . text . name2String $ x
  ppr (T.Forall t) =
    lunbind t $ \(x, b) -> do
      b' <- ppr b
      return $ parens (text "∀" <> (text (name2String x)) <> dot <> b')
  ppr T.IntT = return $ text "int"
  ppr T.BoolT = return $ text "bool"
  ppr (T.Prod t1 t2) = do
    t1' <- ppr t1
    t2' <- ppr t2
    return $ parens (t1' <> text "," <+> t2')
  ppr T.UnitT = return $ text "()"


instance Pretty T.Expr where
  ppr (T.Var x) = return . text . name2String $ x
  ppr (T.App f a) = do
    f' <- ppr f
    a' <- ppr a
    return $ parens (f' <+> a')
  ppr (T.TApp f a) = do
    f' <- ppr f
    a' <- ppr a
    return $ parens (f' <> text "@" <> a')
  ppr (T.Lam bnd) =
    lunbind bnd $ \(x, b) -> do
      b' <- ppr b
      return (parens $ text "λ" <> text (name2String x) <+> dot <+> b')
  ppr (T.BLam bnd) =
    lunbind bnd $ \(x, b) -> do
      b' <- ppr b
      return (parens $ text "Λ" <> text (name2String x) <+> dot <+> b')
  ppr (T.IntV n) = return . text . show $ n
  ppr (T.BoolV True) = return (text "true")
  ppr (T.BoolV False) = return (text "false")
  ppr (T.PrimOp op e1 e2) = do
    e1' <- ppr e1
    e2' <- ppr e2
    op' <- ppr op
    return $ parens (e1' <+> op' <+> e2')
  ppr (T.Pair e1 e2) = do
    e1' <- ppr e1
    e2' <- ppr e2
    return $ parens (e1' <> ", " <+> e2')
  ppr (T.Proj1 e) = do
    e' <- ppr e
    return $ e' <> dot <> text (show 1)
  ppr (T.Proj2 e) = do
    e' <- ppr e
    return $ e' <> dot <> text (show 2)
  ppr T.Unit = return $ text "()"
  ppr (T.If p e1 e2) = do
    p' <- ppr p
    e1' <- ppr e1
    e2' <- ppr e2
    return $ text "if" <+> p' <+> text "then" <+> e1' <+> text "else" <+> e2'
  ppr (T.Let b) = do
    lunbind b $ \(x, (e, body)) -> do
      e' <- ppr e
      b' <- ppr body
      return $ text "let" <+> text (name2String x) <+> text "=" <+> e' <+> text "in" <+> b'

instance Pretty T.UExpr where
  ppr (T.UVar x) = return . text . name2String $ x
  ppr (T.UApp f a) = do
    f' <- ppr f
    a' <- ppr a
    return $ parens (f' <+> a')
  ppr (T.ULam bnd) =
    lunbind bnd $ \(x, b) -> do
      b' <- ppr b
      return (parens $ text "λ" <> text (name2String x) <+> dot <+> b')
  ppr (T.UIntV n) = return . text . show $ n
  ppr (T.UBoolV True) = return (text "true")
  ppr (T.UBoolV False) = return (text "false")
  ppr (T.UStrV b) = return . text $ show b
  ppr (T.UPrimOp op e1 e2) = do
    e1' <- ppr e1
    e2' <- ppr e2
    op' <- ppr op
    return $ parens (e1' <+> op' <+> e2')
  ppr (T.UPair e1 e2) = do
    e1' <- ppr e1
    e2' <- ppr e2
    return $ parens (e1' <> ", " <+> e2')
  ppr (T.UP1 e) = do
    e' <- ppr e
    return $ e' <> dot <> text (show 1)
  ppr (T.UP2 e) = do
    e' <- ppr e
    return $ e' <> dot <> text (show 2)
  ppr T.UUnit = return $ text "()"
  ppr (T.UIf p e1 e2) = do
    p' <- ppr p
    e1' <- ppr e1
    e2' <- ppr e2
    return $ text "if" <+> p' <+> text "then" <+> e1' <+> text "else" <+> e2'
  ppr (T.ULet b) = do
    lunbind b $ \(x, (e, body)) -> do
      e' <- ppr e
      b' <- ppr body
      return $ text "let" <+> text (name2String x) <+> text "=" <+> e' <+> text "in" <+> b'
  ppr (T.UToString e) = do
    e' <- ppr e
    return $ e' <> dot <> text "toString"


pprint :: Pretty a => a -> Doc
pprint = runLFreshM . ppr

warn :: String -> Doc
warn s = dullred . bold $ text "***" <+> text s <> colon

info :: String -> Doc
info = dullyellow . bold . brackets . text
