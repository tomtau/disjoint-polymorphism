{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Source.Desugar
  ( desugar
  , resolveDecls
  , normalizeTmDecl
  , expandType
  , expandTypeForTerm
  ) where

import Protolude hiding (Type)

import Environment
import Source.Syntax
import Unbound.LocallyNameless


desugar :: [Decl] -> [SimpleDecl]
desugar = map go
  where
    go :: Decl -> SimpleDecl
    go (SDecl decl) = decl
    go (TraitDecl trait) = desugarTrait trait


-- Desugar inherits:
-- trait a (x : A, y : B) inherits b & c {self : C => ...}
-- def a (x : A) (y : B) (self : C) = b(self) ,, c(self) ,, {...}
desugarTrait :: Trait -> SimpleDecl
desugarTrait trait =
  (DefDecl $
   TmBind
     tname
     typarams
     ((map (second Just) params) ++ [(s2n self, Just st)])
     -- if no supers, return body
     -- otherwise merge them to body
     (maybe body (flip Merge body) (foldl1May Merge supers))
     (retType trait))
  where
    typarams = traitTyParams trait
    params = traitParams trait
    tb = traitBody trait
    tname = traitName trait
    (self, st) = selfType trait
    supers = traitSuper trait
    tb' = resolveDecls tb -- We substitute away all type declarations in traits
    body = mkRecds (map normalizeTmDecl tb')





-- After parsing, earlier declarations appear first in the list
-- Substitute away all type declarations
resolveDecls :: [SimpleDecl] -> [TmBind]
resolveDecls decls = map (substs substPairs) [decl | (DefDecl decl) <- decls]
  where
    tydecls =
      foldl
        (\ds t -> (substs (toSubst ds) t) : ds)
        []
        ([decl | decl@(TypeDecl {}) <- decls])
    substPairs = toSubst tydecls
    toSubst ds = [((s2n n), t) | TypeDecl (TypeBind n _ t) <- ds]

{-

Translate

Tmdef n [(A, T1), (B, T2)] [(x, A), (y, B)] C e

to

(n, /\ A*T1. B*T2. \x : A .\y : B . (e : C), [T1, T2])

-}

normalizeTmDecl :: TmBind -> (BindName, Expr)
normalizeTmDecl decl = (bindName decl, body)
  where
    body =
      foldr (\(n, s) tm -> DLam (bind (n, Embed s) tm)) fun (bindTyParams decl)
    fun =
      foldr
        (\(n, t) tm ->
           case t of
             Just t' -> LamA (bind (n, Embed t') tm)
             Nothing -> Lam (bind n tm))
        (maybe (bindRhs decl) (Anno (bindRhs decl)) (bindRhsTyAscription decl))
        (bindParams decl)


expandTypeForTerm :: Ctx -> Expr -> Expr
expandTypeForTerm ctx e = runFreshM (expandTypeForTerm' ctx e)

expandTypeForTerm' :: Fresh m => Ctx -> Expr -> m Expr
expandTypeForTerm' d (Anno e t) = do
  t' <- expandType' d t
  return $ Anno e t'
expandTypeForTerm' d (App e1 e2) = do
  e1' <- expandTypeForTerm' d e1
  e2' <- expandTypeForTerm' d e2
  return $ App e1' e2'
expandTypeForTerm' d (Lam b) = do
  (x, e) <- unbind b
  e' <- expandTypeForTerm' d e
  return (Lam (bind x e'))
expandTypeForTerm' d (Let b) = do
  ((x, Embed t), (e1, e2)) <- unbind b
  t' <- expandType' d t
  e1' <- expandTypeForTerm' d e1
  e2' <- expandTypeForTerm' d e2
  return $ Let (bind (x, embed t') (e1', e2'))
expandTypeForTerm' d (DLam b) = do
  ((x, Embed t), e) <- unbind b
  t' <- expandType' d t
  e' <- expandTypeForTerm' d e
  return (DLam (bind (x, embed t') e'))
expandTypeForTerm' d (TApp e t) = do
  e' <- expandTypeForTerm' d e
  t' <- expandType' d t
  return $ TApp e' t'
expandTypeForTerm' d (DRec l e) = do
  e' <- expandTypeForTerm' d e
  return $ DRec l e'
expandTypeForTerm' d (Acc e l) = do
  e' <- expandTypeForTerm' d e
  return $ Acc e' l
expandTypeForTerm' d (Merge e1 e2) = do
  e1' <- expandTypeForTerm' d e1
  e2' <- expandTypeForTerm' d e2
  return $ Merge e1' e2'
expandTypeForTerm' _ (IntV n) = return $ IntV n
expandTypeForTerm' _ (BoolV n) = return $ BoolV n
expandTypeForTerm' _ (StrV n) = return $ StrV n
expandTypeForTerm' d (If e1 e2 e3) = do
  e1' <- expandTypeForTerm' d e1
  e2' <- expandTypeForTerm' d e2
  e3' <- expandTypeForTerm' d e3
  return $ If e1' e2' e3'
expandTypeForTerm' _ Top = return Top
expandTypeForTerm' d (LamA b) = do
  ((x, Embed t), e) <- unbind b
  t' <- expandType' d t
  e' <- expandTypeForTerm' d e
  return $ LamA (bind (x, embed t') e')
expandTypeForTerm' _ (Var v) = return $ Var v
expandTypeForTerm' d (PrimOp op e1 e2) = do
  e1' <- expandTypeForTerm' d e1
  e2' <- expandTypeForTerm' d e2
  return $ PrimOp op e1' e2'



-- | Recursively expand all type synonyms. The given type must be well-kinded.

expandType :: Ctx -> Type -> Type
expandType c t = runFreshM (expandType' c t)

expandType' :: Fresh m => Ctx -> Type -> m Type

-- Interesting cases:
expandType' d (TVar a) = do
  case lookupTVarSynMaybe d a of
    Nothing -> return $ TVar a
    Just t -> expandType' d t
expandType' d (OpAbs b) = do
  (x, t) <- unbind b
  t' <- expandType' (extendTVarCtx x Star d) t
  return $ OpAbs (bind x t')
expandType' d (OpApp t1 t2) = do
  OpAbs b <- expandType' d t1
  t2' <- expandType' d t2
  (x, t) <- unbind b
  return (subst x t2' t)

expandType' _ IntT = return IntT
expandType' _ BoolT = return BoolT
expandType' _ StringT = return StringT
expandType' d (Arr t1 t2) = do
  t1' <- expandType' d t1
  t2' <- expandType' d t2
  return $ Arr t1' t2'
expandType' d (And t1 t2) = do
  t1' <- expandType' d t1
  t2' <- expandType' d t2
  return $ And t1' t2'
expandType' d (DForall b) = do
  ((a, Embed t1), t2) <- unbind b
  t1' <- expandType' d t1
  t2' <- expandType' d t2
  return $ DForall (bind (a, embed t1') t2')
expandType' d (SRecT l t) = do
  t' <- expandType' d t
  return $ SRecT l t'
expandType' _ TopT = return TopT
