
module Main where

import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Lazy
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Test.HUnit
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test
import System.Exit
import System.IO

import Web.Vorple

data Csrf a
  = Csrf { csrfKey :: String, csrfData :: a }
  deriving (Eq, Ord, Read, Show)

$(deriveJSON (Prelude.drop 4) ''Csrf)

data Rec1
  = Rec1a { ax :: Int, ay :: Int }
  | Rec1b { bx :: Int, by :: Int }
  deriving (Eq, Ord, Read, Show)

$(deriveJSON id ''Rec1)

rb :: ByteString -> Session SResponse
rb = srequest . SRequest (defaultRequest { requestMethod = methodPost })

rt :: Text -> Session SResponse
rt = rb . encodeUtf8

rj :: (ToJSON a) => a -> Session SResponse
rj = rb . encode

appEcho :: Application
appEcho = vorple defaultOptions () () $ \r -> return (r :: Rec1)

-- | Assert that the response body is the given JSON object wrapped in a Csrf
-- object, and return the CSRF key string.
assertJsonBody :: (FromJSON a, Eq a, Show a) => a -> SResponse -> Session String
assertJsonBody expBody x = liftIO $ do
  case decode $ simpleBody x of
    Nothing -> do
      assertFailure "Failed to parse response body"
      return ""
    Just (Csrf actKey actBody) -> do
      assertEqual "response body" expBody actBody
      return actKey

assertNoCookie :: SResponse -> Session ()
assertNoCookie = assertNoHeader "Set-Cookie"

testEcho1 = TestLabel "testEcho1" $ TestCase $ flip runSession appEcho $ do
  let b1 = Rec1a 0 1
  r1 <- rj $ Csrf "" b1
  assertStatus 200 r1
  assertNoCookie r1
  void $ assertJsonBody b1 r1
  let b2 = Rec1a 1 0
  r2 <- rj $ Csrf "" b2
  assertStatus 200 r2
  assertNoCookie r2
  void $ assertJsonBody b2 r2

tests = TestList [testEcho1]

main = do
  c <- runTestTT tests
  when (errors c /= 0 || failures c /= 0) exitFailure

