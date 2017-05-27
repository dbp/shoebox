{-# LANGUAGE OverloadedStrings #-}
module Database.Shed.BlobServer.Directory where

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

import Database.Shed.Types
import Database.Shed.BlobServer

data FileStore = FileStore Text

instance BlobServer FileStore where
 writeBlob (FileStore dir) dat = do
   (SHA1 name) <- getBlobName dat
   let filename = T.unpack $ dir <> "/sha1/" <> T.take 2 (T.drop 4 name) <> "/" <> T.take 2 (T.drop 6 name) <> "/" <> name <> ".dat"
   BS.writeFile filename dat
   return (SHA1 name)

 readBlob (FileStore dir) (SHA1 t) = if not ("sha1-" `T.isPrefixOf` t) then error $ T.unpack $ "SHA1 does not start with 'sha1-': " <> t else
  do let filename = dir <> "/sha1/" <> (T.take 2 (T.drop 5 t)) <> "/" <> (T.take 2 (T.drop 7 t)) <> "/" <> t <> ".dat"
     ex <- doesFileExist $ T.unpack filename
     if ex
       then Just <$> BL.readFile (T.unpack filename)
       else return Nothing

 enumerateBlobs (FileStore dir) f = enum [dir <> "/sha1"] 0
  where
    enum :: [Text] -> Int -> IO ()
    enum pth 3 =
      do dat <- BL.readFile $ T.unpack $ T.intercalate "/" pth
         f (SHA1 $ T.takeWhile (/= '.') (last pth)) dat
    enum pth n = do fs <- listDirectory $ T.unpack $ T.intercalate "/" pth
                    mapM_ (\f -> enum (pth <> [T.pack f]) (n+1)) fs
