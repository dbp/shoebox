{-# LANGUAGE OverloadedStrings #-}
module Shed.Importers where

import           Codec.Archive.Zip
import           Control.Arrow              ((***))
import           Control.Logging            (log')
import           Control.Monad.Trans        (liftIO)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import           Data.ByteString.Unsafe     (unsafeUseAsCStringLen)
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

import           Shed.BlobServer
import           Shed.Files
import           Shed.Files
import           Shed.Indexer
import           Shed.Types

process :: BlobServer a => a -> Key -> Connection -> File -> IO ()
process store key conn f =
  case fileContentType f of
    "image/jpeg" -> addFile conn key store f
    "image/png" -> addFile conn key store f
    "application/zip" -> unzipper store key conn f
    typ -> case takeExtension (T.unpack $ fileName f) of
             ".mbox" -> emailextract store key conn f
             ".jpg" -> addFile conn key store f
             ".jpeg" -> addFile conn key store f
             ".png" -> addFile conn key store f
             ".zip" -> unzipper store key conn f
             ext -> log' $ "Don't know how to process files ending in " <> T.pack ext <> " of type " <> typ <> "."


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

emailextract :: BlobServer a => a -> Key -> Connection -> File -> IO ()
emailextract store key conn f = do
  let messages = parseMBox (TL.decodeUtf8 (fileContent f))
  mapM_ (\m -> do log' $ "Adding email '" <> TL.toStrict (fromLine m) <> "'."
                  brefs <- addChunks store (BL.toStrict $ TL.encodeUtf8 $ body m)
                  let email = EmailBlob (TL.toStrict $ fromLine m)
                                       (map (uncurry Header . (TL.toStrict *** TL.toStrict)) (headers m))
                                       (map (uncurry Part) brefs)
                  let emailblob = BL.toStrict $ encodePretty email
                  exists <- statBlob store emailblob
                  if exists then return () else do
                    (SHA1 eref) <- writeBlob store emailblob
                    (pref, permablob) <- addPermanode key store
                    (cref, claimblob) <- setAttribute key store pref "camliContent" eref
                    indexBlob store conn pref permablob
                    indexBlob store conn cref claimblob
                    indexBlob store conn (SHA1 eref) email

        ) messages
