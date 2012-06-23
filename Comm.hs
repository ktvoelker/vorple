
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Comm where

import Data.Aeson.Types
import Web.Scotty

-- TODO replace
type User = Int

-- TODO replace
type Secret = Int

-- TODO replace
type Error = Int

badAuth, badData :: Error
badData = 401
badAuth = 402
badSecret = 403

data Cookie = Cookie User Secret deriving (Eq, Ord, Show)

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

data Handler a b c =
    RequireAuth   (User -> a -> b -> Either Error c)
  | DoesAuth      (a -> b -> Either Error (User, c))
  | NoRequireAuth (a -> b -> Either Error c)

result :: (ToJSON a) => Either Error a -> ActionM ()
result (Left e) = failure e
result (Right d) = success d

-- TODO
success :: (ToJSON a) => a -> ActionM ()
success = const $ return ()

-- TODO
failure :: Error -> ActionM ()
failure = const $ return ()

-- TODO
getCookie :: ActionM (Maybe Cookie)
getCookie = return Nothing

-- TODO
setCookie :: Cookie -> ActionM ()
setCookie = const $ return ()

-- TODO
makeSecret :: ActionM Secret
makeSecret = return 0

runHandler :: (FromJSON b, ToJSON c) => Handler a b c -> a -> ActionM ()
runHandler h p = do
  (j :: b) <- jsonData
  case h of
    NoRequireAuth h -> result $ h p j
    DoesAuth h -> case h p j of
      (Left e) -> failure e
      (Right (u, r)) -> do
        s <- makeSecret
        setCookie $ Cookie u s
        result $ Right r
    RequireAuth h -> do
      c <- getCookie
      case c of
        Nothing -> failure badAuth
        Just (Cookie u s) -> do
          a <- param "secret"
          if a == s
          then result $ h u p j
          else failure badSecret

serve :: (FromJSON b, ToJSON c) => (Target a, Handler a b c) -> ScottyM ()
serve (target, handler) = runTarget target $ runHandler handler

