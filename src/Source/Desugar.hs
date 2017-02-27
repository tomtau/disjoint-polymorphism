{-# LANGUAGE ViewPatterns #-}


module Source.Desugar
  ( desugar
  , resolveDecls
  , normalizeTmDecl
  , expandType
  ) where

import Environment
import Source.Syntax
import Unbound.LocallyNameless


desugar :: (Fresh m) => [Decl] -> m [SimpleDecl]
desugar ds = mapM go ds
  where
    go :: Fresh m => Decl -> m SimpleDecl
    go (SDecl decl) = return decl
    go (TraitDecl trait) = desugarTrait trait


-- We substitute away all type declarations in traits
desugarTrait :: Fresh m => Trait -> m SimpleDecl
desugarTrait trait = do
  (params, tb) <- unbind parasBody
  let tb' = resolveDecls tb
  return
    (DefDecl $
     TmBind
       name
       [] -- traits have no type variables
       ((map (\(a, b) -> (a, unembed b)) params) ++ [(s2n self, st)])
       (mkRecds (map normalizeTmDecl tb'))
       Nothing)
  where
    parasBody = traitParasBody trait
    name = traitName trait
    (self, st) = selfType trait




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

(n, /\ A*T1. B*T2. \x.\y. (e : C), [T1, T2])

-}

normalizeTmDecl :: TmBind -> (String, Expr)
normalizeTmDecl decl = (bindName decl, body)
  where
    body =
      foldr (\(n, s) tm -> DLam (bind (n, Embed s) tm)) fun (bindTyParams decl)
    fun =
      foldr
        (\(n, t) tm -> LamA (bind (n, Embed t) tm))
        (maybe (bindRhs decl) (Anno (bindRhs decl)) (bindRhsTyAscription decl))
        (bindParams decl)



-- | Recursively expand all type synonyms. The given type must be well-kinded.

expandType :: Ctx -> Type -> Type
expandType c t = runFreshM (expandType' c t)

expandType' :: Fresh m => Ctx -> Type -> m Type
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
expandType' d (TVar a) = do
  case lookupTVarSynMaybe d a of
    Nothing -> return $ TVar a
    Just t -> expandType' d t
expandType' d (DForall b) = do
  ((a, Embed t1), t2) <- unbind b
  t1' <- expandType' d t1
  t2' <- expandType' d t2
  return $ DForall (bind (a, embed t1') t2')
expandType' d (SRecT l t) = do
  t' <- expandType' d t
  return $ SRecT l t'
expandType' _ TopT = return TopT
