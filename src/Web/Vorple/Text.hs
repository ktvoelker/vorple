
module Web.Vorple.Text
  ( readMaybe
  , FromJSON(..)
  , ToJSON(..)
  , deriveJSON
  , encodeJSON
  , decodeJSON
  , decodeJSONSource
  , sinkJSON
  , readJSON
  , showJSON
  , Text()
  , packString
  , unpackString
  , encodeUtf8
  , decodeUtf8
  , ByteString()
  , Word8
  , packBytes
  , unpackBytes
  , Base64(..)
  , encodeBase64
  , encodeUrl
  , decodeUrl
  , StrictByteString()
  , strictBytes
  , runResourceT
  , ($=)
  , (=$)
  , ($$)
  ) where

import Data.List
import Data.Maybe

import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (ToJSON(..), FromJSON(..))
import Data.ByteString.Lazy (ByteString())
import Data.Conduit (runResourceT, ($=), (=$), ($$))
import Data.Text.Lazy (Text())
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Data.Word (Word8)

import qualified Data.ByteString

import qualified Codec.Binary.Base64Url as B64
import qualified Codec.Binary.Url as UE
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode as JE
import qualified Data.Aeson.Types as JT
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString.Lazy as BS
import qualified Data.Conduit as C
import qualified Data.Conduit.Attoparsec as CAP
import qualified Data.Foldable as F
import qualified Data.Monoid as M
import qualified Data.Text.Encoding as ES
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.HMAC as HMAC
import qualified GHC.Exts as X

type StrictByteString = Data.ByteString.ByteString

readMaybe :: (Read a) => String -> Maybe a
readMaybe = listToMaybe . map fst . filter (null . snd) . readsPrec 0

encodeJSON :: (ToJSON a) => a -> ByteString
encodeJSON = J.encode

decodeJSON :: (FromJSON a) => ByteString -> Maybe a
decodeJSON = J.decode

decodeJSONSource
  :: (FromJSON a, Monad m, Functor m, C.MonadThrow m)
  => C.Source m StrictByteString
  -> m (Maybe a)
decodeJSONSource = fmap (JT.parseMaybe J.parseJSON) . ($$ sinkJSON)

sinkJSON :: (C.MonadThrow m) => C.Sink StrictByteString m J.Value
sinkJSON = CAP.sinkParser J.json

showJSON :: (ToJSON a) => a -> Text
showJSON = TB.toLazyText . JE.fromValue . J.toJSON

readJSON :: (FromJSON a) => Text -> Maybe a
readJSON xs = do
  value <-
    either (const Nothing) Just
    $ P.parseOnly J.json
    $ ES.encodeUtf8
    $ T.toStrict xs
  case J.fromJSON value of
    J.Error _   -> Nothing
    J.Success o -> Just o

packString :: String -> Text
packString = T.pack

unpackString :: Text -> String
unpackString = T.unpack

packBytes :: [Word8] -> ByteString
packBytes = BS.pack

unpackBytes :: ByteString -> [Word8]
unpackBytes = BS.unpack

newtype Base64 = Base64 { getBase64Bytes :: ByteString } deriving (Eq, Ord)

instance ToJSON Base64 where
  toJSON = toJSON . getBase64Bytes

instance FromJSON Base64 where
  parseJSON = fmap Base64 . parseJSON

encodeBase64 :: [Word8] -> Base64
encodeBase64 = Base64 . encodeUtf8 . T.pack . B64.encode

encodeUrl :: ByteString -> ByteString
encodeUrl = encodeUtf8 . packString . UE.encode . unpackBytes

decodeUrl :: ByteString -> Maybe ByteString
decodeUrl = fmap packBytes . UE.decode . unpackString . decodeUtf8

strictBytes :: ByteString -> StrictByteString
strictBytes = Data.ByteString.concat . BS.toChunks

