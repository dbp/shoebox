{-# LANGUAGE OverloadedStrings #-}
module Shoebox.Blob.Delete where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text           (Text)
import           Data.Time.Clock

import           Shoebox.BlobServer
import           Shoebox.IndexServer
import           Shoebox.Types

data DeleteBlob = DeleteBlob { blobRef   :: SHA224
                             , timestamp :: UTCTime
                             }

instance FromJSON DeleteBlob where
  parseJSON (Object v) = (do t <- v .: "type"
                             if t == ("delete" :: Text) then
                               DeleteBlob <$> v .: "blobRef"
                                          <*> v .: "timestamp"
                               else fail "Not a delete")
  parseJSON invalid    = typeMismatch "DeleteBlob" invalid

instance ToJSON DeleteBlob where
  toJSON (DeleteBlob ref time) = object ["version" .= (1 :: Int)
                                        ,"type" .= ("delete" :: Text)
                                        ,"blobRef" .= ref
                                        ,"timestamp" .= time]


indexBlob :: SomeBlobServer -> SomeIndexServer -> SHA224 -> DeleteBlob -> IO ()
indexBlob store serv sha (DeleteBlob ref _) = do
  -- NOTE(dbp 2018-12-16): These are safe even if the targets aren't the correct type.
  removeUrl serv ref
  removeItem serv ref
