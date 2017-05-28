{-# LANGUAGE OverloadedStrings #-}
module Shed.BlobServer.Memory where

import qualified Crypto.Hash.SHA1     as SHA1
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashTable.IO    as H
import           Data.Text            (Text)

import           Shed.BlobServer
import           Shed.Types

type HashTable k v = H.BasicHashTable k v

data MemoryStore = MemoryStore (HashTable Text ByteString)

instance BlobServer MemoryStore where
 writeBlob (MemoryStore table) dat = do
   (SHA1 name) <- getBlobName dat
   H.insert table name dat
   return (SHA1 name)

 readBlob (MemoryStore table) (SHA1 t) =
   fmap BL.fromStrict <$> H.lookup table t

 enumerateBlobs (MemoryStore table) f =
   H.mapM_ (\(k,v) -> f (SHA1 k) (BL.fromStrict v)) table
