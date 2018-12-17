{-# LANGUAGE OverloadedStrings #-}
module Shoebox.BlobServer.CachingRedis where

import           Control.Monad.Trans  (liftIO)
import qualified Crypto.Hash          as Hash
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Text            (Text)
import qualified Data.Text.Encoding   as T
import           Database.Redis

import           Shoebox.BlobServer
import           Shoebox.Types

data CachingRedisStore = CachingRedisStore Connection SomeBlobServer

instance BlobServer CachingRedisStore where
 writeBlob (CachingRedisStore _ store) = writeBlob store

 readBlob (CachingRedisStore conn store) (SHA224 t) = runRedis conn $ do
   mres <- get (T.encodeUtf8 t)
   case mres of
     Right (Just res) -> return (Just (BL.fromStrict res))
     _ -> do blob <- liftIO $ readBlob store (SHA224 t)
             case blob of
               Nothing -> return Nothing
               Just blob' -> do
                 set (T.encodeUtf8 t) (BL.toStrict blob')
                 return (Just blob')

 enumerateBlobs (CachingRedisStore _ store) = enumerateBlobs store

 deleteBlob (CachingRedisStore conn store) (SHA224 t) = do
   runRedis conn $ del [T.encodeUtf8 t]
   deleteBlob store (SHA224 t)
