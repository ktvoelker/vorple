
module Util
  ( module Util
  , module Control.Monad
  , module Web.Vorple
  , deriveJSON
  , assertBool
  , assertFailure
  , assertEqual
  , assertStatus
  , runSession
  ) where

import qualified Data.ByteString as SBS
import qualified Data.Text as ST
import qualified Data.Text.Encoding as STE

import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Data.Text.Lazy.Encoding
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test
import System.Exit
import System.IO
import Test.HUnit
import Web.Vorple
import Web.Vorple.Text

data Csrf a
  = Csrf { csrfKey :: String, csrfData :: a }
  deriving (Eq, Ord, Read, Show)

$(deriveJSON (Prelude.drop 4) ''Csrf)

defaultPost = defaultRequest { requestMethod = methodPost }

rj :: (ToJSON a) => a -> Session SResponse
rj = srequest . SRequest defaultPost . encode

rjc :: (ToJSON a) => String -> a -> Session SResponse
rjc cookie =
  srequest
  . SRequest
    ( defaultPost
      { requestHeaders
        = ("Cookie", STE.encodeUtf8 $ ST.pack cookie) : requestHeaders defaultPost
      }
    )
  . encode

-- | Assert that the response body is the given JSON object wrapped in a Csrf
-- object, and return the CSRF key string.
assertJsonBody :: (FromJSON a, Eq a, Show a) => a -> SResponse -> Session String
assertJsonBody expBody x = do
  case decode $ simpleBody x of
    Nothing -> do
      liftIO $ assertFailure "Failed to parse response body"
      return ""
    Just (Csrf actKey actBody) -> do
      liftIO $ assertEqual "response body" expBody actBody
      return actKey

assertNoCookie :: SResponse -> Session ()
assertNoCookie = assertNoHeader "Set-Cookie"

assertCookie :: SResponse -> Session String
assertCookie resp = case filter ((== "Set-Cookie") . fst) $ simpleHeaders resp of
  []      -> liftIO (assertFailure "No cookie") >> return ""
  (c : _) -> return $ ST.unpack $ STE.decodeUtf8 $ snd c

sessionTest :: String -> Application -> Session () -> Test
sessionTest n a s = TestLabel n $ TestCase $ runSession s a

multiTest :: String -> IO () -> Test
multiTest n = TestLabel n . TestCase

runTests :: [Test] -> IO ()
runTests tests = do
  c <- runTestTT $ TestList tests
  when (errors c /= 0 || failures c /= 0) exitFailure

