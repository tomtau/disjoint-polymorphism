{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Environment
  ( lookupTy
  , lookupTyVar
  , lookupTyVarMaybe
  , lookupTmDef
  , runTcMonad
  , TcMonad
  , extendVarCtx
  , extendTyVarCtx
  , extendCtxs
  , Ctx(..)
  , emptyCtx
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map.Strict as M
import           Source.Syntax
import           Text.PrettyPrint.ANSI.Leijen hiding (Pretty)
import           Unbound.LocallyNameless


type TcMonad = FreshMT (ReaderT Ctx (Except Doc))

runTcMonad :: Ctx -> TcMonad a -> Either Doc a
runTcMonad env m = runExcept $ runReaderT (runFreshMT m) env


type VarCtx = M.Map TmName Type
type TyCtx = M.Map TyName Type
type BndCtx = M.Map TmName Expr

-- | Environment manipulation and accessing functions
data Ctx = Ctx
  { varCtx :: VarCtx
  , tyCtx :: TyCtx
  , bndCtx :: BndCtx
  } deriving (Show)

emptyCtx :: Ctx
emptyCtx = Ctx {varCtx = M.empty, tyCtx = M.empty, bndCtx = M.empty}

ctxMap :: (VarCtx -> VarCtx)
       -> (TyCtx -> TyCtx)
       -> (BndCtx -> BndCtx)
       -> Ctx
       -> Ctx
ctxMap f1 f2 f3 ctx =
  Ctx
  {varCtx = f1 (varCtx ctx), tyCtx = f2 (tyCtx ctx), bndCtx = f3 (bndCtx ctx)}

-- | Extend the context with a new binding.
extendVarCtx :: TmName -> Type -> Ctx -> Ctx
extendVarCtx v t = ctxMap (M.insert v t) id id

-- | Extend the context with a new binding.
extendTyVarCtx :: TyName -> Type  -> Ctx -> Ctx
extendTyVarCtx v t = ctxMap id (M.insert v t) id

-- | Extend the context with a list of bindings
extendCtxs :: [(TmName, Type)] -> Ctx -> Ctx
extendCtxs tms ctx = foldr (uncurry extendVarCtx) ctx tms

lookupTy
  :: (MonadReader Ctx m, MonadError Doc m)
  => TmName -> m Type
lookupTy v = do
  env <- asks varCtx
  case M.lookup v env of
    Nothing  -> throwError $ text "Not in scope:" <+> text (show v)
    Just res -> return res


lookupTyVar
  :: (MonadReader Ctx m, MonadError Doc m)
  => TyName -> m Type
lookupTyVar v = do
  env <- asks tyCtx
  case M.lookup v env of
    Nothing  -> throwError $ text "Not in scope:" <+> text (show v)
    Just res -> return res

lookupTyVarMaybe :: Ctx -> TyName -> Maybe Type
lookupTyVarMaybe ctx v = M.lookup v (tyCtx ctx)

lookupTmDef
  :: (MonadReader Ctx m)
  => TmName -> m (Maybe Expr)
lookupTmDef v = do
  env <- asks bndCtx
  return $ M.lookup v env
