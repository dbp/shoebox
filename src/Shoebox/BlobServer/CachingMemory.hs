{-# LANGUAGE OverloadedStrings #-}
module Shoebox.BlobServer.CachingMemory where

import qualified Crypto.Hash          as Hash
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashTable.IO    as H
import           Data.Text            (Text)

import           Shoebox.BlobServer
import           Shoebox.Types

type HashTable k v = H.BasicHashTable k v

data CachingMemoryStore = CachingMemoryStore (HashTable Text ByteString) SomeBlobServer

instance BlobServer CachingMemoryStore where
 writeBlob (CachingMemoryStore _ store) = writeBlob store

 readBlob (CachingMemoryStore cache store) (SHA224 t) = do
   mres <- H.lookup cache t
   case mres of
     Just res -> return (Just res)
     Nothing -> do blob <- readBlob store (SHA224 t)
                   case blob of
                     Nothing -> return Nothing
                     Just blob' -> do
                       H.insert cache t blob'
                       return (Just blob')

 enumerateBlobs (CachingMemoryStore _ store) = enumerateBlobs store

 deleteBlob (CachingMemoryStore cache store) (SHA224 t) = do
   H.delete cache t
   deleteBlob store (SHA224 t)
