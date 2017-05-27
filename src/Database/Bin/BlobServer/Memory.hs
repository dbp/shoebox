{-# LANGUAGE OverloadedStrings #-}
module Database.Bin.BlobServer.Directory where

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.HashTable.IO as H
import qualified Crypto.Hash.SHA1 as SHA1

import Database.Bin.Types
import Database.Bin.BlobServer

type HashTable k v = H.BasicHashTable k v

data MemoryStore = MemoryStore (HashTable Text ByteString)

instance BlobServer MemoryStore where
 writeBlob (MemoryStore table) dat = do
   (SHA1 name) <- getBlobName dat
   H.insert table name dat
   return (SHA1 name)

 readBlob (MemoryStore table) (SHA1 t) =
   H.lookup table t

 enumerateBlobs (MemoryStore table) f = 
   H.mapM_ (\(k,v) -> f (SHA1 k) v) table
