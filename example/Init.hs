
module Init
  ( run
  ) where

import Control.Monad.Reader
import Data.Aeson.Types
import qualified Network.Wai.Handler.FastCGI as FastCGI
import Database.PostgreSQL.Simple

import Types
import Web.Vorple

-- This app key is not secure!
opts = defaultOptions { optAppKey = Just [1..32] }

run :: (FromJSON a, ToJSON b) => (a -> Vorple Env Session IO b) -> IO ()
run h =
  connect ConnectInfo
    { connectDatabase = "test"
    , connectHost     = "/Users/karl/Projects/Vorple/pg"
    , connectPort     = 5432
    , connectUser     = "karl"
    , connectPassword = ""
    }
  >>= return . flip (flip (vorpleIO opts) emptySession) h . Env
  >>= FastCGI.run

