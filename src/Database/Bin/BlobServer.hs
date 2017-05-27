{-# LANGUAGE OverloadedStrings #-}
module Database.Bin.BlobServer where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.Text.Encoding as T

import Database.Bin.Types

getBlobName :: ByteString -> IO SHA1
getBlobName dat = do
  let digest = SHA1.hash dat
  let chars = T.decodeUtf8 $ BL.toStrict $ Builder.toLazyByteString $ Builder.byteStringHex digest
  return (SHA1 $ "sha1-" <> chars)

class BlobServer a where
  writeBlob :: a -> ByteString -> IO SHA1
  readBlob :: a -> SHA1 -> IO (Maybe ByteString)
  enumerateBlobs :: a -> (SHA1 -> ByteString -> IO ()) -> IO ()
