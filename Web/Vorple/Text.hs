
module Web.Vorple.Text
  ( Text()
  , packString
  , encodeUtf8
  , decodeUtf8
  , ByteString()
  , Word8
  , unpackBytes
  , Base64(..)
  , encodeBase64
  ) where

import Data.ByteString.Lazy (ByteString())
import Data.Text.Lazy (Text())
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import Data.Word (Word8)

import qualified Codec.Binary.Base64Url as B64
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.HMAC as HMAC

packString :: String -> Text
packString = T.pack

unpackBytes :: ByteString -> [Word8]
unpackBytes = BS.unpack

newtype Base64 = Base64 { getBase64Text :: Text } deriving (Eq, Ord)

encodeBase64 :: [Word8] -> Base64
encodeBase64 = Base64 . T.pack . B64.encode

