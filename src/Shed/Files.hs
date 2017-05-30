{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Shed.Files where

import           Control.Monad              (guard)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Time.Clock
import           Database.PostgreSQL.Simple (Connection)
import           System.Random              (randomIO)
import           Web.Fn                     (File (..))

import           Shed.BlobServer
import           Shed.Indexer
import           Shed.IndexServer
import           Shed.Types

chunkSize :: Int
chunkSize = 1000000

addChunks :: SomeBlobServer -> ByteString -> IO [(SHA1, Int)]
addChunks store bs = do
  let chunks = splitEvery chunkSize bs
  mapM (\p -> (,B.length p) <$> writeBlob store p) chunks

addPermanode :: SomeBlobServer -> Key -> IO (SHA1, Blob)
addPermanode store key = do
  random <- T.pack . show . abs <$> (randomIO :: IO Int)
  let permablob = PermanodeBlob (keyBlobRef key) random
  perma <- blobToSignedJson key permablob
  (, permablob) <$> writeBlob store perma

setAttribute :: SomeBlobServer -> Key -> SHA1 -> Text -> Text -> IO (SHA1, Blob)
setAttribute store key pref name value = do
  now <- getCurrentTime
  let claimblob = SetAttribute (keyBlobRef key) now pref name value
  claim <- blobToSignedJson key claimblob
  (, claimblob) <$> writeBlob store claim

addFile :: SomeBlobServer -> SomeIndexServer -> Key -> File -> IO ()
addFile store serv key file = do
  refs <- addChunks store (BL.toStrict $ fileContent file)
  let parts = map (uncurry Part) refs
  let fileblob = FileBlob (fileName file) parts
  let fileblob' = BL.toStrict $ encodePretty fileblob
  exists <- statBlob store fileblob'
  if exists then return () else do
    (SHA1 fref) <- writeBlob store fileblob'
    (pref, permablob) <- addPermanode store key
    (cref, claimblob) <- setAttribute store key pref "camliContent" fref
    indexBlob store serv pref permablob
    indexBlob store serv cref claimblob
    indexBlob store serv (SHA1 fref) fileblob

splitEvery :: Int -> ByteString -> [ByteString]
splitEvery n bs = if B.length bs <= n then [bs] else
  let (f,r) = B.splitAt n bs in f : splitEvery n r
