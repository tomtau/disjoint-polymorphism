{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Env
  ( lookupTy
  , runTcMonad
  , TcMonad
  , extendCtx
  , addToCtx
  , throwStrErr
  , Context
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Unbound.LocallyNameless


data Context i t = Ctx {env :: [(i, t)]}


emptyCtx :: (Eq i, Show i) => Context i t
emptyCtx = Ctx []


type TcMonad i t = FreshMT (ReaderT (Context i t) (Except String))


runTcMonad
  :: (Eq i, Show i)
  => TcMonad i t a -> Either String a
runTcMonad m = runExcept $ runReaderT (runFreshMT m) emptyCtx


lookupTy
  :: (Eq i, Show i, MonadReader (Context i t) m, MonadError String m)
  => i -> m t
lookupTy v = do
  x <- lookupTyMaybe v
  case x of
    Nothing  -> throwError $ concat ["Not in scope: ", show $ v]
    Just res -> return res


lookupTyMaybe
  :: (Eq i, Show i, MonadReader (Context i t) m, MonadError String m)
  => i -> m (Maybe t)
lookupTyMaybe v = do
  ctx <- asks env
  return (lookup v ctx)


extendCtx
  :: (Eq i, Show i, MonadReader (Context i t) m)
  => (i, t) -> m a -> m a
extendCtx d = local (\ctx -> ctx { env = d : env ctx })

addToCtx :: (i, t) -> Context i t -> Context i t
addToCtx d ctx = ctx {env = d : env ctx}

throwStrErr
  :: MonadError String m
  => String -> m a
throwStrErr s = throwError $ s ++ "\n"
