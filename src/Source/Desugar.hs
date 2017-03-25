{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}


module Source.Desugar
  ( desugar
  , resolveDecls
  , normalizeTmDecl
  , expandType
  ) where

import Protolude hiding (Type)

import Unbound.LocallyNameless

import Environment
import Source.Syntax



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


-- | Recursively expand all type synonyms. The given type must be well-kinded.

expandType :: Ctx -> Type -> TcMonad Type

-- Interesting cases:
expandType d (TVar a) = do
  case lookupTVarSynMaybe d a of
    Nothing -> return $ TVar a
    Just t -> expandType d t
expandType d (OpAbs b) = do
  ((x, Embed k), t) <- unbind b
  t' <- expandType (extendTVarCtx x k d) t
  return $ OpAbs (bind (x, embed k) t')
expandType d typ@(OpApp t1 t2) =
  expandType d t1 >>= \case
    OpAbs b -> do
      t2' <- expandType d t2
      ((x, Embed k), t) <- unbind b
      expandType d (subst x t2' t)
    _ -> return typ

expandType _ NumT = return NumT
expandType _ BoolT = return BoolT
expandType _ StringT = return StringT
expandType d (Arr t1 t2) = do
  t1' <- expandType d t1
  t2' <- expandType d t2
  return $ Arr t1' t2'
expandType d (And t1 t2) = do
  t1' <- expandType d t1
  t2' <- expandType d t2
  return $ And t1' t2'
expandType d (DForall b) = do
  ((a, Embed t1), t2) <- unbind b
  t1' <- expandType d t1
  t2' <- expandType d t2
  return $ DForall (bind (a, embed t1') t2')
expandType d (SRecT l t) = do
  t' <- expandType d t
  return $ SRecT l t'
expandType _ TopT = return TopT
