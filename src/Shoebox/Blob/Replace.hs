{-# LANGUAGE OverloadedStrings #-}
module Shoebox.Blob.Replace where

import           Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock
import Data.Text (Text)
import Data.Monoid ((<>))

import Shoebox.Types
import Shoebox.BlobServer
import Shoebox.IndexServer

data ReplaceBlob = ReplaceBlob { oldRef :: SHA224
                               , newRef :: SHA224
                               , timestamp :: UTCTime
                               }

instance FromJSON ReplaceBlob where
  parseJSON (Object v) = (do t <- v .: "type"
                             if t == ("replace" :: Text) then
                               ReplaceBlob <$> v .: "oldRef"
                                           <*> v .: "newRef"
                                           <*> v .: "timestamp"
                               else fail "Not a replace")
  parseJSON invalid    = typeMismatch "RelpaceBlob" invalid

instance ToJSON ReplaceBlob where
  toJSON (ReplaceBlob old new time) = object ["version" .= (1 :: Int)
                                             ,"type" .= ("replace" :: Text)
                                             ,"oldRef" .= old
                                             ,"newRef" .= new
                                             ,"timestamp" .= time]

indexBlob :: SomeBlobServer -> SomeIndexServer -> SHA224 -> ReplaceBlob -> IO ()
indexBlob store serv sha (ReplaceBlob old new _) = do
  setRedirection serv ("/" <> unSHA224 old) ("/" <> unSHA224 new)
