module Spook.Server.KeyPerm (encryptBackendKey) where

import Database.Persist.Sql (SqlBackend, fromSqlKey, toSqlKey)
import Database.Persist (ToBackendKey, Key)
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Base64.URL as Bs64Url
import qualified Data.ByteString.Base64 as Bs64
import qualified Crypto.Cipher.AES as Aes
import Data.Int (Int64)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)
import Data.Monoid ((<>))

-- Guaranteed Random.
randomKey16B64 :: Text.Text
randomKey16B64 = "Am3/uydlVyKbBX2uI2HFBQ=="

randomKey16 :: Bs.ByteString
randomKey16 =
  case Bs64.decode $ Text.encodeUtf8 randomKey16B64 of
    Right a -> a
    Left e -> error e

aes :: Aes.AES
aes = Aes.initAES randomKey16

encryptBackendKey :: forall record. ToBackendKey SqlBackend record
              => Key record
              -> Text.Text
encryptBackendKey key =
  let intKey :: Int64 = fromSqlKey key
      packedKey :: Bs.ByteString = Data.ByteString.Lazy.toStrict $ Data.ByteString.Builder.toLazyByteString $ Data.ByteString.Builder.int64BE intKey
      paddedKey :: Bs.ByteString = packedKey <> Bs.pack [0, 0, 0, 0, 0, 0, 0, 0]
      permutedKey :: Bs.ByteString = Aes.encryptECB aes paddedKey
  in Text.decodeUtf8 $ Bs64Url.encode permutedKey

-- TODO: Figure out how to unpack and test.
-- decryptBackendKey :: forall record. ToBackendKey SqlBackend record
--               => Text.Text
--               -> Either String (Key record)
-- decryptBackendKey encryptedKey = do
--   permutedKey :: Bs.ByteString <- Bs64Url.decode $ Text.encodeUtf8 encryptedKey
--   let packedKey = Aes.decryptECB aes permutedKey
--       intKey :: Int64 = Bs.unpack packedKey
--   return $ toSqlKey intKey

