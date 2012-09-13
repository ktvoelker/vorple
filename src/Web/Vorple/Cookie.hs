
module Web.Vorple.Cookie where

import qualified Blaze.ByteString.Builder as BB
import qualified Network.HTTP.Types as H

import Control.Monad
import Data.HMAC
import Data.Maybe
import Web.Cookie

import Web.Vorple.Log
import Web.Vorple.Text
import Web.Vorple.Types

getHmacSum :: [Word8] -> Base64 -> ByteString -> Base64
getHmacSum appKey csrfKey appDataBytes =
  encodeBase64
  $ hmac_sha1 appKey
  $ unpackBytes
  $ getBase64Bytes csrfKey `appendBytes` appDataBytes

getCookie :: [Word8] -> H.RequestHeaders -> Internal e IO (Maybe Cookie)
getCookie appKey rs = do
  name <- asksOpt optCookieName
  let
  { hs =
    map (lazyBytes . snd)
    $ filter ((== name) . decodeUtf8 . lazyBytes . fst)
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

setCookie
  :: (Monad m, MonadOptions m)
  => [Word8]
  -> Base64
  -> ByteString
  -> m H.Header
setCookie appKey csrfKey appDataBytes = do
  name <- asksOpt optCookieName
  path <- asksOpt optCookiePath
  secure <- asksOpt optSecureCookie
  return
    $ ("Set-Cookie",)
    $ BB.toByteString
    $ renderSetCookie
    $ def
      { setCookieName = strictBytes $ encodeUtf8 name
      , setCookiePath = fmap (strictBytes . encodeUtf8) path
      , setCookieHttpOnly = True
      , setCookieSecure = secure
      , setCookieValue =
          strictBytes
          $ encodeUrl
          $ encodeUtf8
          $ showJSON
          $ Cookie (getHmacSum appKey csrfKey appDataBytes) csrfKey appDataBytes
      }

