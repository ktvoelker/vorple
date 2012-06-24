
module Types where

import Database.HDBC.PostgreSQL (Connection)

data Env = Env
  { conn :: Connection
  }

