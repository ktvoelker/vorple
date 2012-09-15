
module Util
  ( module Util
  , module Control.Monad.State
  , module Web.Vorple
  , deriveJSON
  , assertBool
  , assertFailure
  , assertEqual
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.DNS.Public as P
import qualified Network.Wai.Test as WT

import Control.Monad.State
import Data.Aeson
import Data.Aeson.TH
import Data.Time.Clock
import Network.HTTP.Types
import Network.Wai
import System.Exit
import Test.HUnit
import Web.CookieJar
import Web.Vorple

data Csrf a
  = Csrf { csrfKey :: String, csrfData :: a }
  deriving (Eq, Ord, Read, Show)

$(deriveJSON (Prelude.drop 4) ''Csrf)

defaultPost :: Request
defaultPost = WT.defaultRequest { requestMethod = methodPost }

ep :: Endpoint
ep = Endpoint
  { epDomain = "localhost"
  , epPath = "/"
  , epHttp = True
  , epSecure = True
  }

type Session a = StateT Jar WT.Session a

runSession :: Maybe P.Rules -> Session a -> Application -> IO a
runSession rules sess = WT.runSession $ evalStateT sess $ emptyJar rules

cookies :: Session [Cookie]
cookies = do
    now <- liftIO getCurrentTime
    jar <- get
    let (cs, jar') = send now ep jar
    put jar'
    return cs

rj :: (ToJSON a) => a -> Session WT.SResponse
rj reqBody = do
    now <- liftIO getCurrentTime
    (cs, jar') <- get >>= return . sendHeaders now ep
    put jar'
    let hs = defaultPost { requestHeaders = cs ++ requestHeaders defaultPost }
    resp <- lift . WT.srequest . WT.SRequest hs . encode $ reqBody
    modify . receiveHeaders now ep . WT.simpleHeaders $ resp
    return resp

assertStatus :: Int -> WT.SResponse -> Session ()
assertStatus n = lift . WT.assertStatus n

-- | Assert that the response body is the given JSON object wrapped in a Csrf
-- object, and return the CSRF key string.
assertJsonBody :: (FromJSON a, Eq a, Show a) => a -> WT.SResponse -> Session String
assertJsonBody expBody xs = do
  case decode . WT.simpleBody $ xs of
    Nothing -> do
      liftIO $ assertFailure "Failed to parse response body"
      return ""
    Just (Csrf actKey actBody) -> do
      liftIO $ assertEqual "response body" expBody actBody
      return actKey

assertNoCookie :: Session ()
assertNoCookie = cookies >>= liftIO . assertEqual "cookies to send" []

d8s = T.unpack . TE.decodeUtf8

assertCookie :: String -> Session String
assertCookie expName = do
    cs <- cookies
    liftIO $ case cs of
      [Cookie{..}] | d8s cName == expName -> return $ d8s cValue
      [] -> assertFailure "No cookies sent" >> return ""
      [_] -> assertFailure "Wrong cookie name sent" >> return ""
      _ -> assertFailure "Multiple cookies sent" >> return ""

sessionTest :: String -> Application -> Session () -> Maybe P.Rules -> Test
sessionTest n a s r = TestLabel n $ TestCase $ runSession r s a

multiTest :: String -> (Maybe P.Rules -> IO ()) -> Maybe P.Rules -> Test
multiTest n f r = TestLabel n . TestCase . f $ r

runTests :: [Maybe P.Rules -> Test] -> IO ()
runTests tests = do
  rulesText <- readFile "data/effective_tld_names.dat"
  let rules = P.parseRules rulesText
  c <- runTestTT . TestList . map ($ Just rules) $ tests
  when (errors c /= 0 || failures c /= 0) exitFailure

