
module Source.Desugar where

import Source.Syntax
import Unbound.LocallyNameless



desugar :: (Fresh m) => [Decl] -> m [SimpleDecl]
desugar ds = mapM go ds
  where
    go :: Fresh m => Decl -> m SimpleDecl
    go (SDecl decl) = return decl
    go (TraitDecl trait) = desugarTrait trait

desugarTrait :: Fresh m => Trait -> m SimpleDecl
desugarTrait trait = do
  (params, tb) <- unbind parasBody
  let (bodyDecls, _) = resolveDecls tb
      declTypes = [(n, t) | TmDef n t _ <- bodyDecls]
      declDefs = [(n, d) | TmDef n _ d <- bodyDecls]
      paramTypes = map (unembed . snd) params
      paramNames = map fst params
      traitType = mkRecdsT declTypes
      traitDef =
        TmDef
          name
          (foldr Arr (Arr st traitType) paramTypes)
          (foldr
             (\n b -> Lam (bind n b))
             (Lam (bind (s2n self) (mkRecds declDefs)))
             paramNames)
  return traitDef
  where
    parasBody = traitParasBody trait
    name = traitName trait
    (self, st) = selfType trait


-- Note: after parsing, earlier declarations appear first in the list
resolveDecls :: [SimpleDecl] -> ([SimpleDecl], [(TyName, Type)])
resolveDecls decls =
  ( map
      (\(TmDef n p t) -> TmDef n (substs substPairs p) (substs substPairs t))
      [decl | decl@(TmDef _ _ _) <- decls]
  , substPairs)
  where
    toSubst ds = [((s2n n), t) | TyDef n _ t <- ds]
    tydecls =
      foldr
        (\(TyDef n p t) ds -> (TyDef n p (substs (toSubst ds) t)) : ds)
        []
        (reverse ([decl | decl@(TyDef _ _ _) <- decls]))
    substPairs = toSubst tydecls
