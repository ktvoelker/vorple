
module Util
  ( module Util
  , module Control.Monad
  , module Web.Vorple
  , deriveJSON
  , assertStatus
  ) where

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

rb :: ByteString -> Session SResponse
rb = srequest . SRequest (defaultRequest { requestMethod = methodPost })

rt :: Text -> Session SResponse
rt = rb . encodeUtf8

rj :: (ToJSON a) => a -> Session SResponse
rj = rb . encode

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

sessionTest :: String -> Application -> Session () -> Test
sessionTest n a s = TestLabel n $ TestCase $ runSession s a

runTests :: [Test] -> IO ()
runTests tests = do
  c <- runTestTT $ TestList tests
  when (errors c /= 0 || failures c /= 0) exitFailure

