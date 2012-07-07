
{-# LANGUAGE OverloadedStrings #-}
module Init
  ( run
  ) where

import Control.Monad.Reader
import Data.Aeson.Types
import qualified Network.Wai.Handler.FastCGI as FastCGI
import Database.HDBC.PostgreSQL

import Types
import Web.Vorple

run :: (FromJSON a, ToJSON b) => (a -> Vorple Env () IO b) -> IO ()
run h =
  connectPostgreSQL "dbname=test;host=/Users/karl/Projects/Vorple/pg"
  >>= flip (flip (runVorpleIO Nothing) ()) h . Env
  >>= FastCGI.run

