
module Main where

import Database.PostgreSQL.Simple

import Init
import Types
import qualified Types.Command as C
import qualified Types.Result as R
import Web.Vorple

main = run $ \cmd -> case cmd of
  C.Echo{..} -> do
    return $ R.Echo number
  C.Incr -> do
    s <- get
    put $ s { count = count s + 1 }
    return $ R.Echo $ count s
  C.Login{..} -> do
    c <- asks conn
    r <- liftIO $ query c
      "SELECT id FROM users WHERE email = ? AND password = CRYPT(?, password)"
      (email, password)
    case r of
      [Only id] -> do
        modify $ \s -> s { user = Just id }
        return R.LoggedIn
      _ -> return R.NotLoggedIn

