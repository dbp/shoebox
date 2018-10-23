{-# LANGUAGE OverloadedStrings #-}
module Shoebox.Importer where

import           Codec.Archive.Zip
import           Control.Arrow              ((***))
import           Control.Logging            (log')
import           Control.Monad              (when)
import           Control.Monad.Trans        (liftIO)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import           Data.ByteString.Unsafe     (unsafeUseAsCStringLen)
import           Data.Char                  (toLower)
import qualified Data.Conduit.Binary        as CB
import           Data.MBox                  (Message (..), parseMBox)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import           Database.PostgreSQL.Simple (Connection)
import           Magic                      (MagicFlag (MagicMimeType),
                                             magicCString, magicLoadDefault,
                                             magicOpen)
import           System.FilePath            (takeExtension)
import           System.IO                  (hClose)
import           System.IO.Temp             (withTempFile)
import           Web.Fn                     (File (..))

import qualified Shoebox.Blob.Email            as Email
import qualified Shoebox.Blob.File             as File
import           Shoebox.BlobServer
import           Shoebox.Indexer
import           Shoebox.IndexServer
import           Shoebox.Types

isBoringFile :: File -> Bool
isBoringFile f = "__MACOSX" `T.isPrefixOf` fileName f || ".DS_STORE" == fileName f

importers :: [SomeBlobServer -> SomeIndexServer -> File -> (File -> IO ()) -> IO ()]
importers =
  [ File.recognizeBlob
  , Email.recognizeBlob
  , unzipper
  ]

process :: SomeBlobServer -> SomeIndexServer -> File -> IO ()
process store serv f =
  if isBoringFile f then return () else
    mapM_ (\imp -> imp store serv f (process store serv)) importers

unzipper :: SomeBlobServer -> SomeIndexServer -> File -> (File -> IO ()) -> IO ()
unzipper store serv f p =
  when (fileContentType f == "application/zip" || (map toLower $ takeExtension (T.unpack $ fileName f)) == ".zip") $ do
    m <- magicOpen [MagicMimeType]
    magicLoadDefault m
    withTempFile "tmp" "zip." $ \tmpFile hFile -> do
      bs <- B.readFile (filePath f)
      B.hPut hFile bs
      hClose hFile
      withArchive tmpFile $ do
        names <- entryNames
        mapM_ (\n -> do bs <- sourceEntry n $ CB.sinkLbs
                        mime <- liftIO $ unsafeUseAsCStringLen (BL.toStrict bs) (magicCString m)
                        liftIO $ withTempFile "tmp" ("zip-" <> (show n) <> ".") $ \tmpFile hFile -> do
                          B.hPut hFile (BL.toStrict bs)
                          hClose hFile
                          p (File (T.pack n) (T.pack mime) tmpFile))
          names
