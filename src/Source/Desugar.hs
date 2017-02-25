{-# LANGUAGE ViewPatterns #-}


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
  return
    (TmDef
       name
       [] -- traits have no type variables
       ((map (\(a, b) -> (a, unembed b)) params) ++ [(s2n self, st)])
       Nothing
       (mkRecds (map normalizeDecl bodyDecls)))
  where
    parasBody = traitParasBody trait
    name = traitName trait
    (self, st) = selfType trait




-- After parsing, earlier declarations appear first in the list
-- Substitute away all type declarations
resolveDecls :: [SimpleDecl] -> ([SimpleDecl], [(TyName, Type)])
resolveDecls decls =
  (map (substs substPairs) [decl | decl@(TmDef {}) <- decls], substPairs)
  where
    tydecls =
      foldl
        (\ds t -> (substs (toSubst ds) t) : ds)
        []
        ([decl | decl@(TyDef {}) <- decls])
    substPairs = toSubst tydecls
    toSubst ds = [((s2n n), t) | TyDef n t <- ds]

{-

Translate

Tmdef n [(A, T1), (B, T2)] [(x, A), (y, B)] C e

to

(n, /\ A*T1. B*T2. \x.\y. (e : C), [T1, T2])

-}

normalizeDecl :: SimpleDecl -> (String, Expr)
normalizeDecl decl = (defName decl, body)
  where
    body =
      foldr (\(n, s) tm -> DLam (bind (n, Embed s) tm)) fun (defTyParams decl)
    fun =
      foldr
        (\(n, t) tm -> LamA (bind (n, Embed t) tm))
        (maybe (defBody decl) (Anno (defBody decl)) (retType decl))
        (defParams decl)
