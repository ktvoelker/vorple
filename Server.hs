
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Server
  ( runServer
  ) where

import qualified Network.Wai.Handler.FastCGI as FastCGI

import Control.Monad.Reader
import Database.HDBC.PostgreSQL
import System.Environment
import Web.Scotty

import Types
import Vorple

runServer :: ReaderT Env ScottyM () -> IO ()
runServer m = do
  args <- getArgs
  let
  { runner = case args of
      ["warp"] -> scotty 3000
      _ -> scottyApp >=> FastCGI.run
  }
  conn <- connectPostgreSQL "dbname=test"
  runner
    $ flip runReaderT (Env conn)
    $ serve (get "/") (return ("Hello, world!" :: String)) >> m

