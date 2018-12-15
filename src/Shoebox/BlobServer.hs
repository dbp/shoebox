{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
module Shoebox.BlobServer where

import qualified Crypto.Hash             as Hash
import           Data.ByteArray          (convert)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy    as BL
import           Data.Maybe              (isJust)
import           Data.Monoid
import qualified Data.Text.Encoding      as T

import           Shoebox.Types

getBlobName :: ByteString -> IO SHA224
getBlobName dat = do
  let digest = convert (Hash.hash dat :: Hash.Digest Hash.SHA224)
  let chars = T.decodeUtf8 $ BL.toStrict $ Builder.toLazyByteString $ Builder.byteStringHex digest
  return (SHA224 $ "sha224-" <> chars)

class BlobServer a where
  statBlob :: a -> ByteString -> IO (Maybe SHA224)
  statBlob store dat = do
    sha <- getBlobName dat
    ex <- isJust <$> readBlob store sha
    return $ if ex then Just sha else Nothing
  writeBlob :: a -> ByteString -> IO SHA224
  readBlob :: a -> SHA224 -> IO (Maybe BL.ByteString)
  enumerateBlobs :: a -> (SHA224 -> BL.ByteString -> IO ()) -> IO ()
  deleteBlob :: a -> SHA224 -> IO ()

data SomeBlobServer = forall s. BlobServer s => SomeBlobServer s

instance BlobServer SomeBlobServer where
  writeBlob (SomeBlobServer s) = writeBlob s
  readBlob (SomeBlobServer s) = readBlob s
  enumerateBlobs (SomeBlobServer s) = enumerateBlobs s
  deleteBlob (SomeBlobServer s) = deleteBlob s
