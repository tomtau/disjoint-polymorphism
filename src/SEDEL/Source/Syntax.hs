{-# LANGUAGE MultiParamTypeClasses
           , TemplateHaskell
           , ScopedTypeVariables
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
#-}

module SEDEL.Source.Syntax where

import SEDEL.Common

import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Unbound.LocallyNameless

-- | Modules
data Module = Module
  { moduleEntries :: [SDecl]
  , mainExpr      :: SDecl
  } deriving (Show)

-- | Declarations other than traits
data SDecl
  = DefDecl TmBind
  | TypeDecl TypeBind
  deriving (Show)

type BindName = String

data Trait = TraitDef
  { selfType      :: (BindName, Type)
    -- ^ Self type
  , traitSuper    :: [Expr]
  , retType       :: Maybe Type
  , traitTyParams :: [(TyName, Type)]
  , traitParams   :: [(TmName, Type)]
  , traitBody     :: [TmBind]
  } deriving (Show)


-- f A1,...,An (x1: t1) ... (xn: tn): t = e
data TmBind = TmBind
  { bindName            :: BindName                  -- f
  , bindTyParams        :: [(TyName, Type)]          -- A1, ..., An
  , bindParams          :: [(TmName, Maybe Type)]    -- x1: t1, ..., xn: tn
  , bindRhs             :: Expr                      -- e
  , bindRhsTyAscription :: Maybe Type                -- t
  } deriving (Show)

-- type T[A1, ..., An] = t
data TypeBind = TypeBind
  { typeBindName   :: BindName           -- T
  , typeBindParams :: [(TyName, Kind)]   -- A1, ..., An
  , typeBindRhs    :: Type               -- t
  } deriving (Show)

-- Unbound library
type TmName = Name Expr
type TyName = Name Type

-- Expression
data Expr = Anno Expr Type
          | Var TmName
          | App Expr Expr
          | Lam (Bind TmName Expr)
          | Let (Bind (TmName, Embed Type) (Expr, Expr))
            -- ^ let expression, possibly recursive
          | DLam (Bind (TyName, Embed Type) Expr)
          | TApp Expr Type
          | DRec Label Expr
          | Acc Expr Label
          | Remove Expr Label Type
          | Merge Expr Expr
          | LitV Double
          | BoolV Bool
          | StrV String
          | PrimOp Operation Expr Expr
          | If Expr Expr Expr
          | Top
          | AnonyTrait Trait
          -- ^ Disappear after desugaring
          | DRec' TmBind
          -- ^ Disappear after desugaring
          | LamA (Bind (TmName, Embed Type) Expr)
          -- ^ Not exposed to users, for internal use
          | Bot
  deriving Show

type Label = String
data Type = NumT
          | BoolT
          | StringT
          | Arr Type Type
          | And Type Type
          | TVar TyName
          | DForall (Bind (TyName, Embed Type) Type)
          | SRecT Label Type
          | TopT
          -- Type synonyms
          | OpAbs (Bind (TyName, Embed Kind) Type)
          -- ^ Type-level abstraction: "type T A = t" becomes "type T = \A : *. t",
          | OpApp Type Type
          -- ^ Type-level application: t1 t2

  deriving Show

-- Kinds k := * | k -> k
data Kind = Star | KArrow Kind Kind deriving (Eq, Show)


-- Unbound library instances
$(derive [''Expr, ''Type, ''SDecl, ''TmBind, ''TypeBind, ''Kind, ''Trait])

instance Alpha Type
instance Alpha Expr
instance Alpha Trait
instance Alpha SDecl
instance Alpha TmBind
instance Alpha TypeBind
instance Alpha Kind

instance Subst Expr Type
instance Subst Expr Kind
instance Subst Expr ArithOp
instance Subst Expr LogicalOp
instance Subst Expr Operation
instance Subst Expr CompOp
instance Subst Expr Trait
instance Subst Expr SDecl
instance Subst Expr TmBind
instance Subst Expr TypeBind

instance Subst Expr Expr where
  isvar (Var v) = Just (SubstName v)
  isvar _ = Nothing

instance Subst Type Expr
instance Subst Type Trait
instance Subst Type Operation
instance Subst Type LogicalOp
instance Subst Type CompOp
instance Subst Type ArithOp
instance Subst Type SDecl
instance Subst Type TmBind
instance Subst Type TypeBind
instance Subst Type Kind

instance Subst Type Type where
  isvar (TVar v) = Just (SubstName v)
  isvar _ = Nothing

topType :: Type -> Bool
topType TopT = True
topType _ = False


-- Utility for parsing

evar :: String -> Expr
evar = Var . s2n

tvar :: String -> Type
tvar = TVar . s2n

ebind :: String -> Expr -> Bind TmName Expr
ebind n = bind (s2n n)

elam :: String -> Expr -> Expr
elam b e = Lam (ebind b e)

dlam :: (String, Type) -> Expr -> Expr
dlam (s, t) b = DLam (bind (s2n s, embed t) b)

tforall :: (String,  Type) -> Type -> Type
tforall (s, t) b = DForall (bind (s2n s, embed t) b)

eapp :: Expr -> Expr -> Expr
eapp = App

etapp :: Expr -> Type -> Expr
etapp = TApp

mkRecds :: [(Label, Expr)] -> Expr
mkRecds [] = Top
mkRecds ((l, e):r) = foldl' (\t (l', e') -> Merge t (DRec l' e')) (DRec l e) r

mkRecds' :: [TmBind] -> Expr
mkRecds' = foldl1 Merge . map DRec'

mkRecdsT :: [(Label, Type)] -> Type
mkRecdsT [] = TopT
mkRecdsT ((l, e):r) = foldl (\t (l', e') -> And t (SRecT l' e')) (SRecT l e) r

mkArr :: Type -> [Type] ->Type
mkArr = foldr Arr

mkForall :: Type -> [(TyName, Embed Type)] -> Type
mkForall = foldr (\b t -> DForall (bind b t))

elet :: String -> Type -> Expr -> Expr -> Expr
elet s t e b = Let (bind (s2n s, embed t) (e, b))

transNew :: Type -> [Expr] -> Expr
transNew t es = elet "self" t (foldl1 Merge es) (evar "self")


{-

Translate

[(A, T1), (B, T2)] [(x, A), (y, B)] C e

to

\/ A*T1. B*T2. A -> B -> C

and

/\ A*T1. B*T2. \x.\y.e

-}

teleToTmBind ::
     [(String, Type)] -> [(String, Maybe Type)] -> Type -> Expr -> (Type, Expr)
-- Ideally for defrec, users should annotate all arguments, but here we assume T
-- if not annotated
teleToTmBind tys tms res e =
  let arr = foldr (\(_, t) tt -> Arr (fromMaybe TopT t) tt) res tms
      tbind = foldr tforall arr tys
      fun = foldr (\(n, _) tm -> elam n tm) e tms
      bfun = foldr dlam fun tys
  in (tbind, bfun)
