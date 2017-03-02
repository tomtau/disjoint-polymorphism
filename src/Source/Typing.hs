{-# LANGUAGE FlexibleContexts, PatternGuards #-}

module Source.Typing
  ( tcModule
  ) where

import           Common
import           Control.Monad
import           Control.Monad.Except
import           Environment
import           Prelude hiding ((<$>))
import           PrettyPrint
import           Source.Desugar
import           Source.Subtyping
import           Source.Syntax
import qualified Target.CBN as TC
import qualified Target.Syntax as T
import           Text.PrettyPrint.ANSI.Leijen hiding (Pretty)
import           Unbound.LocallyNameless


-- Type check a module
tcModule :: Module -> TcMonad (Type, T.UExpr, TC.Env)
tcModule m = do
  let decls = moduleEntries m
  let mainE = mainExpr m
  -- Step 1: Desugar traits
  let sdecls = desugar decls
  -- Step 2: Check module
  targetDecls <- foldr tcM (return ([])) (sdecls ++ [mainE])
  -- Step 3: Generate initial environment for execution
  let (mainType, mainTarget) = last targetDecls
  let declsTarget = map snd . init $ targetDecls
  let initEnv =
        foldl
          (\env (n, e) -> TC.extendCtx (n, e, env) env)
          TC.emptyEnv
          declsTarget
  return (mainType, snd mainTarget, initEnv)
  where
    tcM
      :: SimpleDecl
      -> TcMonad [(Type, (T.UName, T.UExpr))]
      -> TcMonad [(Type, (T.UName, T.UExpr))]
    tcM (DefDecl decl) m = do
      (dbind, transD) <- tcTmDecl decl
      fmap (((snd dbind, transD) :)) $ localCtx ((uncurry extendVarCtx) dbind) m
    tcM (TypeDecl tdecl) m = do
      (n, tdef, k) <- tcTyDecl tdecl
      localCtx (addTypeSynonym n tdef k) m

-- Type check declarations
tcTmDecl :: TmBind -> TcMonad ((TmName, Type), (T.UName, T.UExpr))
tcTmDecl decl = do
  oldDef <- lookupTmDef (s2n n)
  case oldDef of
    Nothing -> do
      (typ, trans) <- infer term
      return ((s2n n, typ), (s2n n, trans))
    Just _ -> throwError $ text "Multiple definitions of" <+> text n
  where
    (n, term) = normalizeTmDecl decl -- term has been annotated, so we can infer

tcTyDecl :: TypeBind -> TcMonad (TyName, Type, Kind)
tcTyDecl (TypeBind n params rhs) = do
  return (s2n n, pullRight params rhs, Star)

-- | Kinding.
kind :: Fresh m => Ctx -> Type -> m (Maybe Kind)
kind d (TVar a) = return $ lookupTVarKindMaybe d a
kind _ IntT = return $ Just Star
kind _ BoolT = return $ Just Star
kind _ StringT = return $ Just Star
kind _ TopT = return $ Just Star
kind d (Arr t1 t2) = justStarIffAllHaveKindStar d [t1, t2]
kind d (And t1 t2) = justStarIffAllHaveKindStar d [t1, t2]
kind d (DForall b) = do
  ((a, _), t) <- unbind b
  kind (extendTVarCtx a Star d) t
kind d (SRecT l t) = justStarIffAllHaveKindStar d [t]

{-
    Δ,x::* ⊢ t :: k
    -------------------- (K-Abs) Restriction compared to F_omega: x can only have kind *
    Δ ⊢ λx. t :: * => k
-}
kind d (OpAbs b) = do
  (x, t) <- unbind b
  maybe_k <- kind (extendTVarCtx x Star d) t
  case maybe_k of
    Nothing -> return Nothing
    Just k  -> return $ Just (KArrow Star k)

{-
    Δ ⊢ t1 :: k11 => k12  Δ ⊢ t2 :: k11
    ------------------------------------ (K-App)
    Δ ⊢ t1 t2 :: k12
-}
kind d (OpApp t1 t2) = do
  maybe_k1 <- kind d t1
  maybe_k2 <- kind d t2
  case (maybe_k1, maybe_k2) of
    (Just (KArrow k11 k12), Just k2)
      | k2 == k11 -> return (Just k12)
    _ -> return Nothing



justStarIffAllHaveKindStar :: Fresh m => Ctx -> [Type] -> m (Maybe Kind)
justStarIffAllHaveKindStar d ts = do
  ps <- mapM (hasKindStar d) ts
  if and ps
    then return $ Just Star
    else return Nothing

hasKindStar :: Fresh m => Ctx -> Type -> m Bool
hasKindStar d t = do
  k <- kind d t
  return (k == Just Star)



-- | "Pull" the type params at the LHS of the equal sign to the right.
-- A (high-level) example:
--   A B t  ->  \A. \B. t
pullRight :: [TyName] -> Type -> Type
pullRight params t = foldr (\n t' -> OpAbs (bind n t')) t params





---------------------------
-- Γ ⊢ e ⇒ A ~> E

-- note: target is untyped
---------------------------

infer :: Expr -> TcMonad (Type, T.UExpr)

{-

Γ ⊢ ⊤ ⇒ ⊤  ~> ()

-}
infer Top = return (TopT, T.UUnit)

infer (IntV n) = return (IntT, T.UIntV n)

infer (BoolV b) = return (BoolT, T.UBoolV b)

infer (StrV b) = return (StringT, T.UStrV b)

{-

   x:A ∈ Γ
---------------
Γ ⊢ x ⇒ A ~> x

-}
infer (Var x) = do
  t <- lookupVarTy x
  return (t, T.UVar (translate x))  -- Change the sort of a name

{-

Γ ⊢ e ⇐ A  ~> E
------------------
Γ ⊢ e : A ⇒ A ~> E

-}
infer (Anno e a) = do
  c <- askCtx
  e' <- check e (expandType c a)
  return (a, e')

{-

Γ ⊢ e1 ⇒ A1 -> A2  ~> E1
Γ ⊢ e2 ⇐ A1        ~> E2
----------------------------
Γ ⊢ e1 e2 ⇒ A2     ~> E1 E2

-}
infer inp@(App e1 e2) = do
  (arr, e1') <- infer e1
  c <- askCtx
  case (expandType c arr) of
    Arr a1 a2 -> do
      e2' <- check e2 a1
      return (a2, T.UApp e1' e2')
    _ ->
      throwError
        (hang 2 $
         text "type of application mismatch in" <+>
         squotes (pprint inp) <> colon <$>
         text "function" <+>
         squotes (pprint e1) <+> text "has type" <+> squotes (pprint arr))

{-

Γ ⊢ e ⇒ ∀(α ∗ B). C  ~> E
Γ ⊢ A
Γ ⊢ A ∗ B
-------------------------------
Γ ⊢ e A ⇒ [α := A] C  ~> E

-}
infer inp@(TApp e a) = do
  wf a
  (t, e') <- infer e
  ctx <- askCtx
  case (expandType ctx t) of
    DForall t' -> do
      ((x, Embed b), c) <- unbind t'
      disjoint ctx (expandType ctx a) (expandType ctx b)
      return (subst x a c, e')
    _ ->
      throwError
        (hang 2 $
         text "type of application mismatch in" <+>
         squotes (pprint inp) <> colon <$>
         text "type-level function" <+>
         squotes (pprint e) <+> text "has type" <+> squotes (pprint t))

{-

Γ ⊢ e1 ⇒ A ~> E1
Γ ⊢ e2 ⇒ B ~> E2
Γ ⊢ A∗B
------------------------------
Γ ⊢ e1,,e2 ⇒ A&B ~> (E1, E2)

-}
infer (Merge e1 e2) = do
  (a, e1') <- infer e1
  (b, e2') <- infer e2
  ctx <- askCtx
  disjoint ctx (expandType ctx a) (expandType ctx b)
  return (And a b, T.UPair e1' e2')

{-

Γ ⊢ e ⇒ A ~> E
----------------------
Γ ⊢{l=e} ⇒ {l:A} ~> E

-}
infer (DRec l e) = do
  (a, e') <- infer e
  return (SRecT l a, e')

{-

Γ ⊢ e ⇒ {l : A} ~> E
----------------------
Γ ⊢ e.l ⇒ A ~> E

The above is what is shown in the paper. In the implementation, we'd like to
avoid annotating a record before projection. The following is the modified rule:

Γ ⊢ e ⇒ t ~> E
t • l = A ~> c
-----------------------
Γ ⊢ e.l ⇒ A ~> c E

-}
infer (Acc e l) =
  if l == "toString" -- ad-hoc extension to "toString" method
    then do
      (_, e') <- infer e
      return (StringT, T.UToString e')
    else do
      (t, e') <- infer e
      c <- askCtx
      case select (expandType c t) l of
        Just (a, c) -> return (a, T.UApp c e')
        _ ->
          throwError
            (hang 2 $
             text "expect a record type with label" <+>
             squotes (text l) <+> text "for" <+> squotes (pprint e) <$>
             text "but got" <+> squotes (pprint t))

{-

Γ ⊢ A
Γ , a * A ⊢ e ⇒ B ~> E
a fresh
---------------------------------
Γ ⊢ Λ(α∗A).e ⇒ ∀(α∗A).B ~> E

-}
infer (DLam t) = do
  ((x, Embed a), e) <- unbind t
  wf a
  (b, e') <- localCtx (extendConstrainedTVarCtx x a) $ infer e
  return (DForall (bind (x, embed a) b), e')

infer (PrimOp op e1 e2) =
  case op of
    Arith _ -> do
      e1' <- check e1 IntT
      e2' <- check e2 IntT
      return (IntT, T.UPrimOp op e1' e2')
    Logical _ -> do
      e1' <- check e1 IntT
      e2' <- check e2 IntT
      return (BoolT, T.UPrimOp op e1' e2')
    Append -> do
      e1' <- check e1 StringT
      e2' <- check e2 StringT
      return (StringT, T.UPrimOp op e1' e2')

infer inp@(If e1 e2 e3) = do
  e1' <- check e1 BoolT
  (t2, e2') <- infer e2
  (t3, e3') <- infer e3
  if aeq t2 t3
    then return (t2, T.UIf e1' e2' e3')
    else throwError $
         (hang 2 $
          text "if branches type mismatch in" <+> squotes (pprint inp) <> colon <$>
          squotes (pprint e2) <+> text "has type" <+> squotes (pprint t2) <$>
          squotes (pprint e3) <+> text "has type" <+> squotes (pprint t3))

{-

Γ, x:t ⊢ e1 ⇐ t ~> e1'
Γ, x:t ⊢ e2 ⇒ t' ~> e2'
-----------------------------------------------------
Γ ⊢ let x : t = e1 in e2 ⇒ t' ~> let x = e1' in e2'

Note: Recursive let binding

-}
infer (Let b) = do
  ((x, Embed t), (e1, e2)) <- unbind b
  e1' <- localCtx (extendVarCtx x t) $ check e1 t
  (t', e2') <- localCtx (extendVarCtx x t) $ infer e2
  return (t', T.ULet (bind (translate x) (e1', e2')))



infer (LamA t) = do
  ((x, Embed a), e) <- unbind t
  wf a
  (b, e') <- localCtx (extendVarCtx x a) $ infer e
  return (Arr a b, T.ULam (bind (translate x) e'))


infer a = throwError $ text "Infer not implemented:" <+> pprint a




------------------------
-- Γ ⊢ e ⇐ A ~> E
------------------------

check :: Expr -> Type -> TcMonad T.UExpr

{-

Γ ⊢ A
Γ , x:A ⊢ e ⇐ B ~> E
---------------------------
Γ ⊢ λx. e ⇐ A → B ~> λx. E

-}
check (Lam l) (Arr a b) = do
  (x, e) <- unbind l
  wf a
  e' <- localCtx (extendVarCtx x a) $ check e b
  return (T.ULam (bind (translate x) e'))

-- check inp@(Lam _) t =
--   throwError $
--   text "expect an arrow type for " <+>
--   squotes (pprint inp) <+> text "but got" <+> squotes (pprint t)


{-


Γ ⊢ A
Γ , a * A ⊢ e ⇐ B ~> E
---------------------------------
Γ ⊢ Λ(α∗A).e ⇐ ∀(α∗A).B ~> E


-}
check (DLam l) (DForall b) = do
  t <- unbind2 l b
  case t of
    Just ((x, Embed a), e, _, b) -> do
      wf a
      localCtx (extendConstrainedTVarCtx x a) $ check e b
    Nothing -> throwError $ text "Patterns have different binding variables"

-- check inp@(DLam _) t =
--   throwError $
--   text "expect a forall type" <+>
--   squotes (pprint inp) <+> text "but got" <+> squotes (pprint t)

{-

Γ ⊢ e1 ⇐ A ~> E1
Γ ⊢ e2 ⇐ B ~> E2
Γ ⊢ A∗B
------------------------------
Γ ⊢ e1,,e2 ⇐ A&B ~> (E1, E2)

-}
check (Merge e1 e2) (And a b) = do
  e1' <- check e1 a
  e2' <- check e2 b
  ctx <- askCtx
  disjoint ctx (expandType ctx a) (expandType ctx b)
  return (T.UPair e1' e2')

{-

Γ ⊢ e ⇐ A ~> E
----------------------
Γ ⊢{l=e} ⇐ {l:A} ~> E

-}

check (DRec l e) (SRecT l' a) = do
  when (l /= l') $
    throwError (text "Labels not equal" <+> text l <+> text "and" <+> text l')
  check e a


{-

Γ ⊢ e ⇒ A ~> E
A <: B ~> c
Γ ⊢ B
-------------------
Γ ⊢ e ⇐ B ~> c E

-}

check e b = do
  wf b
  (a, e') <- infer e
  ctx <- askCtx
  let res = subtype ctx a b
  case res of
    Right c -> do
      return (T.UApp c e')
    Left err ->
      throwError
        (hang 2 $
         text "subtyping failed" <> colon <$>
         squotes (pprint e) <+> text "has type" <+> squotes (pprint a) <$>
         text "which is not a subtype of" <+> squotes (pprint b))


-- | Check that a (expanded) type is well-formed: disjoint and has kind *.
wf :: Type -> TcMonad ()
wf t = do
  ctx <- askCtx
  let t' = expandType ctx t
  maybe_kind <- kind ctx t'
  case maybe_kind of
    Nothing -> throwError $ squotes (pprint t) <+> text "is not well-kinded"
    Just Star -> wf' t'
    Just k ->
      throwError
        (hang 2 $
         text "expect type" <+>
         squotes (pprint t) <+> text "has kind star" <$>
         text "but got" <+> squotes (pprint k))


wf' :: Type -> TcMonad ()
wf' IntT = return ()
wf' BoolT = return ()
wf' StringT = return ()
wf' (Arr a b) = wf' a >> wf' b
wf' (And a b) = do
  wf' a
  wf' b
  ctx <- askCtx
  disjoint ctx a b
wf' (TVar x) = lookupTVarConstraint x >> return ()
wf' (DForall t) = do
  ((x, Embed a), b) <- unbind t
  wf' a
  localCtx (extendConstrainedTVarCtx x a) $ wf' b
wf' (SRecT _ t) = wf' t
wf' TopT = return ()
wf' t = throwError $ text "type" <+> pprint t <+> text "is not well-formed"

-- Careful, am I following strictly Fig.3?
disjoint :: (Fresh m, MonadError Doc m) => Ctx -> Type -> Type -> m ()
disjoint _ TopT _ = return ()
disjoint _ _ TopT = return ()

disjoint ctx (TVar x) b
  | Just a <- lookupTVarConstraintMaybe ctx x
  , Right _ <- subtype ctx a b = return ()
disjoint ctx b (TVar x)
  | Just a <- lookupTVarConstraintMaybe ctx x
  , Right _ <- subtype ctx a b = return ()

disjoint ctx (TVar x) (TVar y) =
  throwError $
  text "Type variables:" <+>
  text (name2String x) <+>
  text "and" <+> text (name2String y) <+> text "are not disjoint"

disjoint ctx (DForall t) (DForall t') = do
  t <- unbind2 t t'
  case t of
    Just ((x, Embed a1), b, (_, Embed a2), c) ->
      disjoint (extendConstrainedTVarCtx x (And a1 a2) ctx) b c
    _ -> throwError $ text "Patterns have different binding variables"

disjoint ctx (SRecT l a) (SRecT l' b) =
  if l == l'
    then disjoint ctx a b
    else return ()

disjoint ctx (Arr _ a2) (Arr _ b2) = disjoint ctx a2 b2
disjoint ctx (And a1 a2) b = do
  disjoint ctx a1 b
  disjoint ctx a2 b
disjoint ctx a (And b1 b2) = do
  disjoint ctx a b1
  disjoint ctx a b2
disjoint _ IntT IntT = throwError $ text "Int and Int are not disjoint"
disjoint _ BoolT BoolT = throwError $ text "Bool and Bool are not disjoint"
disjoint _ StringT StringT = throwError $ text "String and String are not disjoint"
disjoint _ _ _ = return ()


--------------------
-- τ1 • l = τ2  → C
--------------------
select :: Type -> Label -> Maybe (Type, T.UExpr)
select t l =
  case t of
    (And t1 t2) ->
      let res1 =
            select t1 l >>= \(t', c) ->
              return (t', T.elam "x" (T.UApp c (T.UP1 (T.evar "x"))))
          res2 =
            select t2 l >>= \(t', c) ->
              return (t', T.elam "x" (T.UApp c (T.UP2 (T.evar "x"))))
      in mplus res1 res2
    (SRecT l' t) ->
      if l == l'
        then Just (t, T.elam "x" (T.evar "x"))
        else Nothing
    _ -> Nothing

-- transTyp :: Fresh m => Type -> m T.Type
-- transTyp IntT = return T.IntT
-- transTyp BoolT = return T.BoolT
-- transTyp (Arr t1 t2) = do
--   t1' <- transTyp t1
--   t2' <- transTyp t2
--   return (T.Arr t1' t2')
-- transTyp (And t1 t2) = do
--   t1' <- transTyp t1
--   t2' <- transTyp t2
--   return (T.Prod t1' t2')
-- transTyp (DForall t) = do
--   ((x, _), body) <- unbind t
--   b <- transTyp body
--   return (T.Forall (bind (translate x) b))
-- transTyp (SRecT _ t) = transTyp t
-- transTyp TopT = return T.UnitT
-- transTyp (TVar x) = return . T.TVar . translate $ x
