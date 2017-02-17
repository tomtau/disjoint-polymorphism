{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Environment
  ( lookupTy
  , lookupTyVar
  , lookupTmDef
  , lookupTyDef
  , runTcMonad
  , TcMonad
  , extendCtx
  , extendCtxs
  , throwStrErr
  , Env
  , emptyEnv
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Source.Syntax
import Unbound.LocallyNameless

import Data.Maybe (listToMaybe, catMaybes, isJust, fromJust)


type TcMonad = FreshMT (ReaderT Env (Except String))

runTcMonad :: Env -> TcMonad a -> Either String a
runTcMonad env m = runExcept $ runReaderT (runFreshMT m) env


-- | Type declarations
data Hint = Hint TyName Type

-- | Environment manipulation and accessing functions
data Env = Env { ctx :: [Decl] }


emptyEnv :: Env
emptyEnv = Env {ctx = []}


lookupTy
  :: (MonadReader Env m, MonadError String m)
  => TmName -> m Type
lookupTy v = do
  x <- lookupTyMaybe v
  case x of
    Nothing  -> throwError $ concat ["Not in scope: ", show $ v]
    Just res -> return res


lookupTyMaybe
  :: MonadReader Env m
  => TmName -> m (Maybe Type)
lookupTyMaybe v = do
  ctx <- asks ctx
  return $ listToMaybe [ty | TmDef v' ty _ <- ctx, v == v' ]

lookupTyVar
  :: (MonadReader Env m, MonadError String m)
  => TyName -> m Type
lookupTyVar v = do
  x <- lookupTyVarMaybe v
  case x of
    Nothing  -> throwError $ concat ["Not in scope: ", show $ v]
    Just res -> return res

lookupTyVarMaybe
  :: MonadReader Env m
  => TyName -> m (Maybe Type)
lookupTyVarMaybe v = do
  ctx <- asks ctx
  return $ listToMaybe [ty | TyDef v' ty _ <- ctx, v == v' ]

lookupTmDef
  :: (MonadReader Env m)
  => TmName -> m (Maybe Expr)
lookupTmDef v = do
  ctx <- asks ctx
  return $ listToMaybe [fromJust d | TmDef v' t d <- ctx, v == v', isJust d]


lookupTyDef
  :: (MonadReader Env m)
  => TyName -> m (Maybe Type)
lookupTyDef v = do
  ctx <- asks ctx
  return $ listToMaybe [fromJust d | TyDef v' t d <- ctx, v == v', isJust d]


-- | Extend the context with a new binding.
extendCtx :: (MonadReader Env m) => Decl -> m a -> m a
extendCtx d = local (\m@(Env {ctx = cs}) -> m {ctx = d : cs})

-- | Extend the context with a list of bindings
extendCtxs :: (MonadReader Env m) => [Decl] -> m a -> m a
extendCtxs ds = local (\m@(Env {ctx = cs}) -> m {ctx = ds ++ cs})

throwStrErr
  :: MonadError String m
  => String -> m a
throwStrErr s = throwError $ s ++ "\n"
