
module Web.Vorple.Cookie where

import qualified Blaze.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Encoding as STE
import qualified Data.Text.Lazy as T
import qualified Network.HTTP.Types as H
import qualified Network.Wai as W

import Control.Monad
import Control.Monad.Trans
import Data.HMAC
import Data.List
import Data.List.Split
import Data.Maybe
import Web.Cookie

import Web.Vorple.Class
import Web.Vorple.Log
import Web.Vorple.Text
import Web.Vorple.Types

cookieName = STE.encodeUtf8 "s"

getHmacSum :: [Word8] -> Base64 -> ByteString -> Base64
getHmacSum appKey csrfKey appDataBytes =
  encodeBase64
  $ hmac_sha1 appKey
  $ unpackBytes
  $ getBase64Bytes csrfKey `BS.append` appDataBytes

getCookie :: [Word8] -> H.RequestHeaders -> Internal e IO (Maybe Cookie)
getCookie appKey rs = do
  let
  { hs =
    map (lazyBytes . snd)
    $ filter ((== cookieName) . fst)
    $ concatMap (parseCookies . snd)
    $ filter ((== "Cookie") . fst) rs
  }
  $(say "POSSIBLE COOKIES:")
  mapM_ $(say "%b") hs
  let cookies = catMaybes $ map (decodeUrl >=> decodeJSON) hs :: [Cookie]
  $(say "PARSED COOKIES:")
  mapM_ $(say "%j") cookies
  let
  { cookieSums =
      map (\Cookie{..} -> getHmacSum appKey cCsrfKey cAppData == cHmacSum) cookies
  }
  $(say "COOKIE SUMS ACCEPTED:")
  mapM_ $(say "%j") cookieSums
  return $ listToMaybe cookies

makeCookie
  :: (Monad m, MonadOptions m)
  => [Word8]
  -> Base64
  -> ByteString
  -> m ByteString
makeCookie appKey csrfKey appDataBytes = do
  secure <- asksOpt optSecureCookies
  return
    $ BB.toLazyByteString
    $ renderSetCookie
    $ def
      { setCookieName = cookieName
      , setCookiePath = Just $ STE.encodeUtf8 "/"
      , setCookieHttpOnly = True
      , setCookieSecure = secure
      , setCookieValue =
          -- TODO is this the best way to accomplish the encoding?
          strictBytes
          $ encodeUrl
          $ encodeUtf8
          $ showJSON
          $ Cookie (getHmacSum appKey csrfKey appDataBytes) csrfKey appDataBytes
      }

setCookie :: ByteString -> H.ResponseHeaders -> H.ResponseHeaders
setCookie = (:) . ("Set-Cookie",) . strictBytes

