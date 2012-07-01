
{-# LANGUAGE OverloadedStrings #-}
module Init
  ( run
  ) where

import Control.Monad.Reader
import Data.Aeson.Types
import qualified Network.Wai.Handler.FastCGI as FastCGI
import Database.HDBC.PostgreSQL

import Types
import Vorple

run :: (FromJSON a) => (a -> Vorple Env ()) -> IO ()
run h = connectPostgreSQL "dbname=test" >>= flip runVorple h . Env >>= FastCGI.run

