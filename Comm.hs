
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Comm where

import Control.Monad.Trans
import Data.Aeson.Types
import qualified Data.Text.Lazy as Text
import GHC.Exts (fromString)
import Network.HTTP.Types (Status(), unauthorized401)
import System.Random
import Web.Scotty

newtype User = User { userId :: Int } deriving (Eq, Ord, Read, Show)

newtype Secret = Secret { secretValue :: Int } deriving (Eq, Ord, Read, Show)

data Cookie = Cookie User Secret deriving (Eq, Ord, Read, Show)

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
    RequireAuth   (User -> a -> b -> IO c)
  | DoesAuth      (a -> b -> IO (User, c))
  | NoRequireAuth (a -> b -> IO c)

result :: (ToJSON a) => IO a -> ActionM ()
result r = liftIO r >>= json

-- TODO
-- Find a Cookie header in requestHeaders for the session key and parse out the body
getCookie :: ActionM (Maybe Cookie)
getCookie = return Nothing

setCookie :: Cookie -> ActionM ()
setCookie = header "Set-Cookie" . ("session=" `Text.append`) . fromString . show

makeSecret :: ActionM Secret
makeSecret = liftIO (getStdRandom random) >>= return . Secret

runHandler :: (FromJSON b, ToJSON c) => Handler a b c -> a -> ActionM ()
runHandler h p = do
  (j :: b) <- jsonData
  case h of
    NoRequireAuth h -> result $ h p j
    DoesAuth h -> do
      (u, r) <- liftIO $ h p j
      s <- makeSecret
      setCookie $ Cookie u s
      json r
    RequireAuth h -> do
      c <- getCookie
      case c of
        Nothing -> status unauthorized401
        Just (Cookie u s) -> do
          a <- param "secret"
          if Secret a == s
          then result $ h u p j
          else status unauthorized401

serve :: (FromJSON b, ToJSON c) => (Target a, Handler a b c) -> ScottyM ()
serve (target, handler) = runTarget target $ runHandler handler

