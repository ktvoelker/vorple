
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
module Web.Vorple.Class where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Network.HTTP.Types (Status(), status500)

import Web.Vorple.Text
import Web.Vorple.Types

instance (Monad m) => Monad (OptionsT m) where
  return = OptionsT . return
  OptionsT m >>= f = OptionsT $ m >>= getOptionsT . f
  fail = OptionsT . fail

instance MonadTrans OptionsT where
  lift = OptionsT . lift

instance (MonadIO m) => MonadIO (OptionsT m) where
  liftIO = OptionsT . liftIO

instance (MonadReader r m) => MonadReader r (OptionsT m) where
  ask = lift ask
  local f = OptionsT . mapReaderT (local f) . getOptionsT
  reader = lift . reader

instance (MonadState s m) => MonadState s (OptionsT m) where
  get = lift get
  put = lift . put

instance Error Status where
  noMsg = status500

instance (Monad m) => Monad (Vorple e s m) where
  return = Vorple . return
  Vorple m >>= f = Vorple $ m >>= getVorple . f
  fail = Vorple . fail

instance MonadTrans (Vorple e s) where
  lift = Vorple . lift . lift . lift . lift . lift

instance (Monad m) => MonadError Status (Vorple e s m) where
  throwError     = Vorple . throwError
  catchError m f = Vorple $ getVorple m `catchError` (getVorple . f)

instance (Monad m) => MonadReader e (Vorple e s m) where
  ask     = Vorple ask
  local f = Vorple . local f . getVorple
  reader  = Vorple . reader

instance (Monad m) => MonadState s (Vorple e s m) where
  get = Vorple get
  put = Vorple . put

instance (Monad m) => MonadWriter ByteString (Vorple e s m) where
  writer = Vorple . writer
  tell   = Vorple . tell
  listen = Vorple . listen . getVorple
  pass   = Vorple . pass . getVorple

instance (MonadIO m) => MonadIO (Vorple e s m) where
  liftIO = Vorple . liftIO

