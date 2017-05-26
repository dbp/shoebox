{-# LANGUAGE OverloadedStrings #-}
module Database.Bin.BlobServer where

import Data.ByteString (ByteString)

import Database.Bin.Types

class BlobServer a where
  writeBlob :: a -> ByteString -> IO SHA1
  readBlob :: a -> SHA1 -> IO (Maybe ByteString)
  enumerateBlobs :: a -> (SHA1 -> ByteString -> IO ()) -> IO ()
