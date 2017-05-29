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
import           Shed.Types


chunkSize :: Int
chunkSize = 1000000

addChunks :: BlobServer a => a -> ByteString -> IO [(SHA1, Int)]
addChunks store bs = do
  let chunks = splitEvery chunkSize bs
  mapM (\p -> (,B.length p) <$> writeBlob store p) chunks

addPermanode :: BlobServer a => Key -> a -> IO (SHA1, Blob)
addPermanode key store = do
  random <- T.pack . show . abs <$> (randomIO :: IO Int)
  let permablob = PermanodeBlob (keyBlobRef key) random
  perma <- blobToSignedJson key permablob
  (, permablob) <$> writeBlob store perma

setAttribute :: BlobServer a => Key -> a -> SHA1 -> Text -> Text -> IO (SHA1, Blob)
setAttribute key store pref name value = do
  now <- getCurrentTime
  let claimblob = SetAttribute (keyBlobRef key) now pref name value
  claim <- blobToSignedJson key claimblob
  (, claimblob) <$> writeBlob store claim

addFile :: BlobServer a => Connection -> Key -> a -> File -> IO ()
addFile conn key store file = do
  refs <- addChunks store (BL.toStrict $ fileContent file)
  let parts = map (uncurry Part) refs
  let fileblob = FileBlob (fileName file) parts
  let fileblob' = BL.toStrict $ encodePretty fileblob
  exists <- statBlob store fileblob'
  if exists then return () else do
    (SHA1 fref) <- writeBlob store fileblob'
    (pref, permablob) <- addPermanode key store
    (cref, claimblob) <- setAttribute key store pref "camliContent" fref
    indexBlob store conn pref permablob
    indexBlob store conn cref claimblob
    indexBlob store conn (SHA1 fref) fileblob

splitEvery :: Int -> ByteString -> [ByteString]
splitEvery n bs = if B.length bs <= n then [bs] else
  let (f,r) = B.splitAt n bs in f : splitEvery n r
