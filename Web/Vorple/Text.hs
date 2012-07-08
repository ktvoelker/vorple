
module Web.Vorple.Text
  ( FromJSON(..)
  , ToJSON(..)
  , deriveJSON
  , encodeJSON
  , decodeJSON
  , Text()
  , packString
  , encodeUtf8
  , decodeUtf8
  , ByteString()
  , Word8
  , unpackBytes
  , Base64(..)
  , encodeBase64
  ) where

import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (ToJSON(..), FromJSON(..))
import Data.ByteString.Lazy (ByteString())
import Data.Text.Lazy (Text())
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Data.Word (Word8)

import qualified Codec.Binary.Base64Url as B64
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.HMAC as HMAC

encodeJSON :: (ToJSON a) => a -> ByteString
encodeJSON = J.encode

decodeJSON :: (FromJSON a) => ByteString -> Maybe a
decodeJSON = J.decode

packString :: String -> Text
packString = T.pack

unpackBytes :: ByteString -> [Word8]
unpackBytes = BS.unpack

newtype Base64 = Base64 { getBase64Text :: Text } deriving (Eq, Ord)

instance ToJSON Base64 where
  toJSON = toJSON . T.toStrict . getBase64Text

instance FromJSON Base64 where
  parseJSON = fmap (Base64 . T.fromStrict) . parseJSON

encodeBase64 :: [Word8] -> Base64
encodeBase64 = Base64 . T.pack . B64.encode

