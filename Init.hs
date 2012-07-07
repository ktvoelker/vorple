
{-# LANGUAGE OverloadedStrings #-}
module Init
  ( run
  ) where

import Control.Monad.Reader
import Data.Aeson.Types
import qualified Network.Wai.Handler.FastCGI as FastCGI
import Database.PostgreSQL.Simple

import Types
import Web.Vorple

run :: (FromJSON a, ToJSON b) => (a -> Vorple Env Session IO b) -> IO ()
run h =
  connect ConnectInfo
    { connectDatabase = "test"
    , connectHost     = "/Users/karl/Projects/Vorple/pg"
    , connectPort     = 5432
    , connectUser     = "karl"
    , connectPassword = ""
    }
  >>= flip (flip (runVorpleIO Nothing) emptySession) h . Env
  >>= FastCGI.run

