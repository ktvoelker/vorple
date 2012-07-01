
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Server
  ( runServer
  ) where

import qualified Network.Wai.Handler.FastCGI as FastCGI

import Control.Monad.Reader
import Data.Aeson.Types
import Database.HDBC.PostgreSQL
import System.Environment
import Web.Scotty

import Types
import Vorple

runServer :: (FromJSON a) => (a -> Vorple Env ()) -> IO ()
runServer handler = do
  conn <- connectPostgreSQL "dbname=test"
  run (Env conn) handler >>= FastCGI.run

