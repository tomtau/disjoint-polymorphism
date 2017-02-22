{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Environment
  ( lookupTy
  , lookupTyVar
  , lookupTmDef
  -- , lookupTyDef
  , runTcMonad
  , TcMonad
  , extendVarCtx
  , extendTyVarCtx
  , extendCtx
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

extendCtx :: Decl -> Ctx -> Ctx
extendCtx (TmDef x t _) = extendVarCtx x t
extendCtx (TyDef x t _) = extendTyVarCtx x t

-- | Extend the context with a list of bindings
extendCtxs :: [Decl] -> Ctx -> Ctx
extendCtxs ds ctx = foldr extendCtx ctx ds

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

lookupTmDef
  :: (MonadReader Ctx m)
  => TmName -> m (Maybe Expr)
lookupTmDef v = do
  env <- asks bndCtx
  return $ M.lookup v env
