
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

rb :: ByteString -> Request
rb = simpleRequest . SRequest (defaultRequest { requestMethod = methodPost })

rt :: Text -> Request
rt = rb . encodeUtf8

rj :: (ToJSON a) => a -> Request
rj = rb . encode

appEcho :: Application
appEcho = vorple defaultOptions () () $ \r -> return (r :: Rec1)

testEcho1 = TestLabel "testEcho1" $ TestCase $ flip runSession appEcho $ do
  let body = Rec1a 0 1
  liftIO $ System.IO.hPutStr stderr "Hello, world! 1\n"
  liftIO $ Data.ByteString.Lazy.hPutStr stderr $ encode $ Csrf "" $ Rec1a 0 1
  let sr = rj $ Csrf "" $ Rec1a 0 1
  liftIO $ System.IO.hPutStr stderr "Hello, world! 2\n"
  x <- request sr
  liftIO $ Data.ByteString.Lazy.hPutStr stderr $ simpleBody x
  assertStatus 200 x
  assertNoHeader "Set-Cookie" x
  let Just (Csrf key _) = decode $ simpleBody x :: Maybe (Csrf Rec1)
  assertBody (encode $ Csrf key body) x

tests = TestList [testEcho1]

main = do
  c <- runTestTT tests
  when (errors c /= 0 || failures c /= 0) exitFailure

