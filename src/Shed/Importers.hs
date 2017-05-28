{-# LANGUAGE OverloadedStrings #-}
module Shed.Importers where

import           Codec.Archive.Zip
import           Control.Logging            (log')
import           Control.Monad.Trans        (liftIO)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import           Data.ByteString.Unsafe     (unsafeUseAsCStringLen)
import qualified Data.Conduit.Binary        as CB
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple (Connection)
import           Magic                      (MagicFlag (MagicMimeType),
                                             magicCString, magicLoadDefault,
                                             magicOpen)
import           System.IO                  (hClose)
import           System.IO.Temp             (withTempFile)
import           Web.Fn                     (File (..))

import           Shed.BlobServer
import           Shed.Files
import           Shed.Files
import           Shed.Types

process :: BlobServer a => a -> Key -> Connection -> File -> IO ()
process store key conn f =
  case fileContentType f of
    "image/jpeg" -> addFile conn key store f
    "application/zip" -> unzipper store key conn f
    typ -> log' $ "Don't know how to process files of type " <> typ <> "."


unzipper :: BlobServer a => a -> Key -> Connection -> File -> IO ()
unzipper store key conn f = do
  m <- magicOpen [MagicMimeType]
  magicLoadDefault m
  withTempFile "tmp" "zip." $ \tmpFile hFile -> do
    B.hPut hFile (BL.toStrict $ fileContent f)
    hClose hFile
    withArchive tmpFile $ do
      names <- entryNames
      mapM_ (\n -> do bs <- sourceEntry n $ CB.sinkLbs
                      mime <- liftIO $ unsafeUseAsCStringLen (BL.toStrict bs) (magicCString m)
                      liftIO $ process store key conn (File (T.pack n) (T.pack mime) bs)) names
