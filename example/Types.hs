
module Types where

import Data.Aeson.TH
import Database.PostgreSQL.Simple

data Env = Env
  { conn :: Connection
  }

data Session = Session
  { user  :: Maybe Int
  , count :: Int
  } deriving (Eq)

emptySession :: Session
emptySession = Session Nothing 0

$(deriveJSON id ''Session)

