{-# LANGUAGE OverloadedStrings #-}
module Database.Bin.BlobServer.Directory where

import Data.Monoid
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import System.Directory (doesFileExist, listDirectory)

import Database.Bin.Types
import Database.Bin.BlobServer

data FileStore = FileStore Text

instance BlobServer FileStore where
 writeBlob (FileStore dir) dat = do
  let digest = SHA1.hash dat
  let chars = T.decodeUtf8 $ BL.toStrict $ Builder.toLazyByteString $ Builder.byteStringHex digest
  let filename = T.unpack $ dir <> "/sha1/" <> T.take 2 chars <> "/" <> T.take 2 (T.drop 2 chars) <> "/sha1-" <> chars <> ".dat"
  BS.writeFile filename dat
  return (SHA1 $ "sha1-" <> chars)

 readBlob (FileStore dir) (SHA1 t) = if not ("sha1-" `T.isPrefixOf` t) then error $ T.unpack $ "SHA1 does not start with 'sha1-': " <> t else
  do let filename = dir <> "/sha1/" <> (T.take 2 (T.drop 4 t)) <> "/" <> (T.take 2 (T.drop 6 t)) <> "/" <> t <> ".dat"
     ex <- doesFileExist $ T.unpack filename
     if ex
       then Just <$> BS.readFile (T.unpack filename)
       else return Nothing

 enumerateBlobs (FileStore dir) f = enum [dir <> "/sha1"] 0
  where
    enum :: [Text] -> Int -> IO ()
    enum pth 3 =
      do dat <- BS.readFile $ T.unpack $ T.intercalate "/" pth
         f (SHA1 $ T.takeWhile (/= '.') (last pth)) dat
    enum pth n = do fs <- listDirectory $ T.unpack $ T.intercalate "/" pth
                    mapM_ (\f -> enum (pth <> [T.pack f]) (n+1)) fs
