
module Web.Vorple.Cookie where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T
import qualified Network.HTTP.Types as H
import qualified Network.Wai as W

import Control.Monad
import Control.Monad.Trans
import Data.HMAC
import Data.List
import Data.List.Split
import Data.Maybe

import Web.Vorple.Log
import Web.Vorple.Text
import Web.Vorple.Types

cookiePrefix :: Text
cookiePrefix = "s="

cookieSuffix :: Text
cookieSuffix = "; path=/"

cookiePrefixBytes :: ByteString
cookiePrefixBytes = encodeUtf8 cookiePrefix

cookieSuffixBytes :: ByteString
cookieSuffixBytes = encodeUtf8 cookieSuffix

getHmacSum :: [Word8] -> Base64 -> ByteString -> Base64
getHmacSum appKey csrfKey appDataBytes =
  encodeBase64
  $ hmac_sha1 appKey
  $ unpackBytes
  $ getBase64Bytes csrfKey `BS.append` appDataBytes

semicolon :: Word8
semicolon = case unpackBytes $ encodeUtf8 ";" of
  [n] -> n
  _   -> undefined

getCookie :: [Word8] -> H.RequestHeaders -> Internal e IO (Maybe Cookie)
getCookie appKey rs = do
  let
  { hs =
      map (BS.drop (BS.length cookiePrefixBytes))
      $ filter (cookiePrefixBytes `BS.isPrefixOf`)
      $ concatMap (BS.split semicolon . BS.fromChunks . (: []) . snd)
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

makeCookie :: [Word8] -> Base64 -> ByteString -> ByteString
makeCookie appKey csrfKey appDataBytes =
  BS.append cookiePrefixBytes
  $ flip BS.append cookieSuffixBytes
  $ encodeUrl
  -- TODO is this the best way to accomplish the encoding?
  $ encodeUtf8
  $ showJSON
  $ Cookie (getHmacSum appKey csrfKey appDataBytes) csrfKey appDataBytes

setCookie :: ByteString -> H.ResponseHeaders -> H.ResponseHeaders
setCookie = (:) . ("Set-Cookie",) . strictBytes

