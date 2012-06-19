
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Comm where

import Web.Scotty

-- TODO replace
type Json = String

-- TODO
parseJson :: String -> Maybe Json
parseJson _ = Nothing

-- TODO replace
type User = Int

-- TODO replace
type Error = Int

badAuth, badData :: Error
badData = 401
badAuth = 402

data Method = Get | Post | Put | Delete deriving (Eq, Ord, Enum, Bounded, Show)

data UrlPart a = Static String | Variable (String -> a -> a)

data Target a = Target
  { method      :: Method
  , url         :: [UrlPart a]
  , emptyParams :: a
  }

-- TODO
runTarget :: Target a -> (a -> ActionM ()) -> ScottyM ()
runTarget _ _ = return ()

data Handler a =
    RequireAuth   (User -> a -> Json -> Either Error Json)
  | DoesAuth      (a -> Json -> Either Error (User, Json))
  | NoRequireAuth (a -> Json -> Either Error Json)

-- TODO
result :: Either Error Json -> ActionM ()
result = const $ return ()

-- TODO
getUser :: ActionM (Maybe User)
getUser = return Nothing

-- TODO
setUser :: User -> ActionM ()
setUser = const $ return ()

runHandler :: Handler a -> a -> ActionM ()
runHandler h p = do
  d <- param "data"
  let j = parseJson d
  case j of
    Nothing -> result $ Left badData
    Just j -> case h of
      NoRequireAuth h -> result $ h p j
      DoesAuth h -> case h p j of
        (Left e) -> result $ Left e
        (Right (u, r)) -> do
          -- TODO make a secret and include it in the cookie
          setUser u
          -- TODO wrap r with a new secret
          result $ Right r
      RequireAuth h -> do
        -- TODO unwrap the secret from the data and check it
        u <- getUser
        case u of
          Nothing -> result $ Left badAuth
          Just u -> result $ h u p j

serve :: (forall a. (Target a, Handler a)) -> ScottyM ()
serve (target, handler) = runTarget target $ runHandler handler

serveAll :: [forall a. (Target a, Handler a)] -> ScottyM ()
serveAll = mapM_ serve

