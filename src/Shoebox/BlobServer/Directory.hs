{-# LANGUAGE OverloadedStrings #-}
module Shoebox.BlobServer.Directory where

import           Control.Logging
import           Control.Monad           (filterM, void, when)
import qualified Crypto.Hash             as Hash
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy    as BL
import           Data.List               (isPrefixOf)
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           System.Directory        (createDirectoryIfMissing,
                                          doesDirectoryExist, doesFileExist,
                                          listDirectory, removeFile)

import           Shoebox.BlobServer
import           Shoebox.Types

data FileStore = FileStore Text

getDir :: Text -> Text -> FilePath
getDir dir name = T.unpack $ dir <> "/sha224/" <> T.take 2 (T.drop 7 name) <> "/" <> T.take 2 (T.drop 9 name) <> "/"

instance BlobServer FileStore where
 writeBlob (FileStore dir) dat = do
   (SHA224 name) <- getBlobName dat
   log' $ "WRITE " <> name
   let holder = getDir dir name
   createDirectoryIfMissing True holder
   let filename = holder <> T.unpack name <> ".dat"
   BS.writeFile filename dat
   return (SHA224 name)

 readBlob (FileStore dir) (SHA224 t) = if not ("sha224-" `T.isPrefixOf` t) then error $ T.unpack $ "SHA224 does not start with 'sha224-': " <> t else
  do log' $ "READ " <> t
     let filename = getDir dir t <> T.unpack t <> ".dat"
     ex <- doesFileExist filename
     if ex
       then Just <$> BL.readFile filename
       else return Nothing

 enumerateBlobs (FileStore dir) f = do
   log' "ENUMERATE"
   void $ iterateAllPaths [dir <> "/sha224"] 0 $ \pth ->
      do dat <- BL.readFile $ T.unpack $ T.intercalate "/" pth
         f (SHA224 $ T.takeWhile (/= '.') (last pth)) dat
         return []

 deleteBlob (FileStore dir) (SHA224 t) =
   do log' $ "DELETE " <> t
      let filename = getDir dir t <> T.unpack t <> ".dat"
      ex <- doesFileExist filename
      when ex (removeFile filename)

getAllBlobRefs :: FileStore -> IO [SHA224]
getAllBlobRefs (FileStore dir) =
  iterateAllPaths [dir <> "/sha224"] 0 $ \pth ->
    return [SHA224 $ T.takeWhile (/= '.') (last pth)]

iterateAllPaths :: [Text] -> Int -> ([Text] -> IO [a]) -> IO [a]
iterateAllPaths pth 3 f = f pth
iterateAllPaths pth n f = do
  let dir = T.unpack $ T.intercalate "/" pth
  fs <- listDirectory dir
  let fs' = filter (not . isPrefixOf ".") fs
  concat <$> mapM (\fi -> iterateAllPaths (pth <> [T.pack fi]) (n+1) f) fs'
