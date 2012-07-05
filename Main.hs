
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Convertible (convert)

import qualified DB
import Init
import Types
import qualified Types.Command as C
import qualified Types.Result as R
import Web.Vorple

main = run $ \cmd -> case cmd of
  C.Echo{..} -> do
    return $ R.Echo number
  C.Login{..} -> do
    sth <- DB.prepare
      "SELECT id FROM users WHERE email = ? AND password = CRYPT(?, password)"
    DB.execute sth [convert email, convert password]
    [id] <- DB.mustFetchRow sth
    -- let id' = convert id
    -- setUser id'
    return R.LoggedIn

