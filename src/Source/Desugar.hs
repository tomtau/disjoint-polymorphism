
module Source.Desugar where

import Source.Syntax
import Unbound.LocallyNameless



desugar :: Trait -> [Decl]
desugar (TraitDef name p a st tb) = [traitType, traitDef]
  where
    traitType :: Decl
    traitType = TyDef (s2n (maybe name id a)) TopT undefined
    traitDef :: Decl
    traitDef = undefined



-- Note: after parsing, earlier declarations appear first in the list
resolveDecls :: [Decl] -> ([Decl], [(TyName, Type)])
resolveDecls decls =
  ( map
      (\(TmDef n p t) -> TmDef n (substs substPairs p) (substs substPairs t))
      [decl | decl@(TmDef _ _ _) <- decls]
  , substPairs)
  where
    toSubst ds = [(n, t) | TyDef n _ t <- ds]
    tydecls =
      foldr
        (\(TyDef n p t) ds -> (TyDef n p (substs (toSubst ds) t)) : ds)
        []
        (reverse ([decl | decl@(TyDef _ _ _) <- decls]))
    substPairs = toSubst tydecls
