
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import qualified Network.Wai.Handler.FastCGI as FastCGI

import Control.Monad.Reader
import Database.HDBC.PostgreSQL
import System.Environment
import Web.Scotty

import Handlers
import Types
import Vorple

main = do
  args <- getArgs
  let
  { runner = case args of
      ["warp"] -> scotty 3000
      _ -> scottyApp >=> FastCGI.run
  }
  conn <- connectPostgreSQL "dbname=test"
  runner $ flip runReaderT (Env conn) $ do
    serve (get "/") $ return ("Hello, world!" :: String)
    serve (post "/munge") munge

