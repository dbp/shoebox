{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Shoebox.BlobServer.S3 where

import           Control.Exception            (SomeException, catch)
import           Control.Lens                 (each, view, (&), (.~))
import           Control.Monad                (void)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (ResourceT)
import qualified Crypto.Hash                  as Hash
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Lazy         as BL
import           Data.Conduit                 (($$+-))
import           Data.Conduit.Binary          (sinkLbs)
import qualified Data.HashTable.IO            as H
import           Data.Text                    (Text)
import           Network.AWS                  (Credentials (Discover), Env,
                                               LogLevel (Error), Logger,
                                               Region (NorthVirginia),
                                               envLogger, newEnv, newLogger,
                                               runAWS, runResourceT, send,
                                               within)
import           Network.AWS.Data.Body        (RsBody (..), toBody)
import           Network.AWS.S3               (BucketName (..), ObjectKey (..),
                                               getObject, gorsBody, putObject)
import           Network.AWS.S3.DeleteObject  (deleteObject)
import           Network.AWS.S3.ListObjects   (listObjects, loMarker,
                                               lorsContents, lorsIsTruncated)
import           Network.AWS.S3.Types         (oKey, _ObjectKey)
import           System.IO                    (stdout)

import           Shoebox.BlobServer
import           Shoebox.Types

data S3Store = S3Store BucketName

instance BlobServer S3Store where
 writeBlob (S3Store bucket) dat = do
   (SHA224 name) <- getBlobName dat
   lgr  <- newLogger Error stdout
   env  <- newEnv Discover
   runResourceT $ runAWS (env & envLogger .~ lgr) $
     within NorthVirginia $
     send (putObject bucket (ObjectKey name) (toBody dat))
   return (SHA224 name)

 readBlob (S3Store bucket) (SHA224 t) = do
   lgr  <- newLogger Error stdout
   env  <- newEnv Discover
   catch (do contents <- runResourceT $ do
               (RsBody body) <- runAWS (env & envLogger .~ lgr) $
                                within NorthVirginia $ do
                                 rs <- send (getObject bucket (ObjectKey t))
                                 return (view gorsBody rs)
               body $$+- sinkLbs
             return $ Just contents)
          (\(e :: SomeException) -> return Nothing)

 enumerateBlobs (S3Store bucket) f = do
     lgr  <- newLogger Error stdout
     env  <- newEnv Discover
     void $ runResourceT $ iterateAllRefs bucket env lgr Nothing iterateResponse
   where iterateResponse :: Env -> Logger -> [SHA224] -> ResourceT IO ()
         iterateResponse env lgr refs =
           mapM_ (\(SHA224 ref) -> do
             (RsBody body) <- runAWS (env & envLogger .~ lgr) $
                                within NorthVirginia $ do
                                 rs <- send (getObject bucket (ObjectKey ref))
                                 return (view gorsBody rs)
             b <- body $$+- sinkLbs
             liftIO $ f (SHA224 ref) b)
           refs

 deleteBlob (S3Store bucket) (SHA224 t) = do
   lgr  <- newLogger Error stdout
   env  <- newEnv Discover
   catch (do runResourceT $ do
               runAWS (env & envLogger .~ lgr) $
                 within NorthVirginia $ do
                 send (deleteObject bucket (ObjectKey t))
             return ())
         (\(e :: SomeException) -> return ())

getAllBlobRefs :: S3Store -> IO [SHA224]
getAllBlobRefs (S3Store bucket) = do
  lgr  <- newLogger Error stdout
  env  <- newEnv Discover
  fmap concat $ runResourceT $ iterateAllRefs bucket env lgr Nothing (\_ _ refs -> return refs)

iterateAllRefs :: BucketName -> Env -> Logger -> Maybe SHA224 -> (Env -> Logger -> [SHA224] -> ResourceT IO a) -> ResourceT IO [a]
iterateAllRefs bucket env lgr starting f = do
  (obs,trunc) <- runAWS (env & envLogger .~ lgr) $
    within NorthVirginia $ do
      rs <- send (listObjects bucket & loMarker .~ (fmap unSHA224 starting))
      return $ (map (SHA224 . view (oKey . _ObjectKey)) (view lorsContents rs)
               ,view lorsIsTruncated rs)
  res <- f env lgr obs
  rest <- if trunc == Just True then iterateAllRefs bucket env lgr (Just (last obs)) f else return []
  return (res:rest)
