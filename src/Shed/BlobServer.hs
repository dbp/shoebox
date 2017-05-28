{-# LANGUAGE OverloadedStrings #-}
module Shed.BlobServer where

import qualified Crypto.Hash.SHA1        as SHA1
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy    as BL
import           Data.Monoid
import qualified Data.Text.Encoding      as T

import           Shed.Types

getBlobName :: ByteString -> IO SHA1
getBlobName dat = do
  let digest = SHA1.hash dat
  let chars = T.decodeUtf8 $ BL.toStrict $ Builder.toLazyByteString $ Builder.byteStringHex digest
  return (SHA1 $ "sha1-" <> chars)

class BlobServer a where
  writeBlob :: a -> ByteString -> IO SHA1
  readBlob :: a -> SHA1 -> IO (Maybe BL.ByteString)
  enumerateBlobs :: a -> (SHA1 -> BL.ByteString -> IO ()) -> IO ()
