{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Shed.Files where

import           Control.Monad              (guard)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
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

addFile :: BlobServer a => Connection -> Key -> a -> File -> IO ()
addFile conn key store file = do
  let chunks = splitEvery chunkSize (BL.toStrict $ fileContent file)
  refs <- mapM (\p -> (,B.length p) <$> writeBlob store p) chunks
  let parts = map (uncurry Part) refs
  let fileblob = FileBlob (fileName file) parts
  let fileblob' = BL.toStrict $ encodePretty fileblob
  (SHA1 fref) <- writeBlob store fileblob'
  random <- T.pack . show . abs <$> (randomIO :: IO Int)
  let permablob = PermanodeBlob (keyBlobRef key) random
  perma <- blobToSignedJson key permablob
  pref <- writeBlob store perma
  now <- getCurrentTime
  let claimblob = SetAttribute (keyBlobRef key) now pref "camliContent" fref
  claim <- blobToSignedJson key claimblob
  cref <- writeBlob store claim
  indexBlob store conn pref permablob
  indexBlob store conn cref claimblob
  indexBlob store conn (SHA1 fref) fileblob

splitEvery :: Int -> ByteString -> [ByteString]
splitEvery n bs = if B.length bs <= n then [bs] else
  let (f,r) = B.splitAt n bs in f : splitEvery n r
