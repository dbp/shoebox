{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports   #-}
module Shoebox.BlobServer.S3 where

import           Control.Exception            (SomeException, catch)
import qualified Control.Exception.Lifted     as Lifted (SomeException, catch)
import           Control.Lens                 (each, view, (&), (.~))
import           Control.Logging              (log')
import           Control.Monad                (void, (<=<))
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (ResourceT)
import qualified "crypton" Crypto.Hash                  as Hash
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Lazy         as BL
import           Data.Conduit                 ((.|), yield, runConduit)
import           Data.Conduit.Binary          (sinkLbs)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.HashTable.IO            as H
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)

import qualified Aws
import qualified Aws.Aws as Aws
import qualified Aws.Core as Aws
import qualified Aws.S3 as S3
import           Control.Monad.Trans.Resource
import           Data.Conduit ((.|), runConduit)
import           Data.Conduit.Binary (sinkFile)
import           Network.HTTP.Conduit (newManager, tlsManagerSettings, responseBody, requestBodySourceChunkedIO)
import           System.IO                    (stdout)

import           Shoebox.BlobServer
import           Shoebox.Types

data S3Store = S3Store S3.Bucket

instance BlobServer S3Store where
 writeBlob (S3Store bucket) dat = do
   (SHA224 name) <- getBlobName dat
   log' $ "WRITE " <> name
   Just creds <- Aws.loadCredentialsDefault
   let cfg = Aws.Configuration Aws.Timestamp creds (Aws.defaultLog Aws.Debug) Nothing
   let s3cfg = S3.s3v4 Aws.HTTP "s3.amazonaws.com" False S3.SignWithEffort
   let body = requestBodySourceChunkedIO (yield dat)
   runResourceT $ do
     Aws.simpleAws cfg s3cfg $
       S3.putObject bucket name body
   return (SHA224 name)

 readBlob (S3Store bucket) (SHA224 t) = do
   log' $ "READ " <> t
   Just creds <- Aws.loadCredentialsDefault
   let cfg = Aws.Configuration Aws.Timestamp creds (Aws.defaultLog Aws.Debug) Nothing
   let s3cfg = S3.s3v4 Aws.HTTP "s3.amazonaws.com" False S3.SignWithEffort
   mgr <- newManager tlsManagerSettings
   catch (do contents <- runResourceT $ do
               (S3.GetObjectMemoryResponse _ rsp) <- Aws.memoryAws cfg s3cfg mgr $
                 S3.getObject bucket t
               return (responseBody rsp)
             return (Just contents))
         (\(e :: SomeException) -> return Nothing)

 enumerateBlobs (S3Store bucket) f = do
     log' "ENUMERATE"
     Just creds <- Aws.loadCredentialsDefault
     let cfg = Aws.Configuration Aws.Timestamp creds (Aws.defaultLog Aws.Debug) Nothing
     let s3cfg = S3.s3v4 Aws.HTTP "s3.amazonaws.com" False S3.SignWithEffort
     mgr <- newManager tlsManagerSettings
     runResourceT $ do
       let src = Aws.awsIteratedSource cfg s3cfg mgr (S3.getBucket bucket)
       let applyF [] = return ()
           applyF (o:os) =
             do
               (S3.GetObjectMemoryResponse _ rsp) <- Aws.memoryAws cfg s3cfg mgr (S3.getObject bucket o)
               liftIO $ f (SHA224 o) (responseBody rsp)
               applyF os
       src `C.connect` CL.mapM_ (applyF . map S3.objectKey . S3.gbrContents <=< Aws.readResponseIO)
       return ()

 deleteBlob (S3Store bucket) (SHA224 t) = do
   log' $ "DELETE " <> t
   Just creds <- Aws.loadCredentialsDefault
   let cfg = Aws.Configuration Aws.Timestamp creds (Aws.defaultLog Aws.Debug) Nothing
   let s3cfg = S3.s3v4 Aws.HTTP "s3.amazonaws.com" False S3.SignWithEffort
   runResourceT $ do
     _ <- Aws.simpleAws cfg s3cfg $ S3.deleteObjects bucket [t]
     return ()

getAllBlobRefs :: S3Store -> IO [SHA224]
getAllBlobRefs (S3Store bucket) = do
  Just creds <- Aws.loadCredentialsDefault
  let cfg = Aws.Configuration Aws.Timestamp creds (Aws.defaultLog Aws.Debug) Nothing
  let s3cfg = S3.s3v4 Aws.HTTP "s3.amazonaws.com" False S3.SignWithEffort
  mgr <- newManager tlsManagerSettings
  runResourceT $ do
    let src = Aws.awsIteratedSource cfg s3cfg mgr (S3.getBucket bucket)
    src `C.connect` CL.foldM (\lst res -> do obs <- Aws.readResponseIO res
                                             return $ lst ++ (map (SHA224 . S3.objectKey) $ S3.gbrContents obs))
                             []
