{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Configuration.Dotenv
import           Control.Logging
import           Control.Monad                (void, when)
import qualified Data.ByteString.Lazy         as BL
import           Data.Maybe                   (fromJust, isNothing)
import           Data.Monoid                  ((<>))
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import qualified Data.Text                    as T
import           Network.AWS.S3               (BucketName (..))
import           Network.Wreq
import qualified Network.Wreq.Session         as Sess
import           System.Directory             (doesFileExist)
import           System.Environment           (lookupEnv)

import           Shoebox.BlobServer
import qualified Shoebox.BlobServer.Directory as Directory
import qualified Shoebox.BlobServer.S3        as S3
import           Shoebox.Types

main :: IO ()
main = withStderrLogging $ Sess.withSession $ \sess -> do
  de <- doesFileExist ".env"
  when de $ loadFile False ".env"

  s3' <- fmap T.pack <$> lookupEnv "S3"
  when (isNothing s3') $ error "S3 environment var must be set to bucket name"
  let s3 = fromJust s3'
  pth' <- fmap T.pack <$> lookupEnv "BLOBS"
  when (isNothing pth') $ error "BLOBS environment var must be set to directory of local blobs"
  let pth = fromJust pth'

  serv <- fmap T.pack <$> lookupEnv "SERVER"

  let foreignIndex =
        case serv of
          Nothing -> \_ -> return ()
          Just url ->
            \sha -> void $ Sess.get sess (T.unpack $ url <> "/" <> (unSHA224 sha) <> "/reindex")

  let s3store = S3.S3Store (BucketName s3)
  s3refs' <- S3.getAllBlobRefs s3store
  let s3refs = Set.fromList s3refs'
  let dirstore = Directory.FileStore pth
  dirrefs' <- Directory.getAllBlobRefs dirstore
  let dirrefs = Set.fromList dirrefs'

  let fors3 = Set.difference dirrefs s3refs
  let fordir = Set.difference s3refs dirrefs

  mapM (\sha -> do log' $ "[" <> pth <> " --> s3://" <> s3 <> "] " <> unSHA224 sha
                   Just bdy <- readBlob dirstore sha
                   writeBlob s3store (BL.toStrict bdy)
                   foreignIndex sha)
    (Set.toList fors3)

  mapM (\sha -> do log' $ "[s3://" <> s3 <> " --> " <> pth <> "] " <> unSHA224 sha
                   Just bdy <- readBlob s3store sha
                   writeBlob dirstore (BL.toStrict bdy))
    (Set.toList fordir)

  return ()


