{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shoebox.Deletion where

import Data.Text (Text)
import           Data.Aeson
import           Data.Aeson.Types
import Data.Time.Clock

import Shoebox.Blob.File
import           Shoebox.Types
import Shoebox.BlobServer

data DeleteBlob = DeleteBlob { blobRef :: SHA224
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


delete :: SomeBlobServer -> IO ()
delete store = enumerateBlobs store $
  \sha content -> case decode content of
                    Nothing -> return ()
                    Just (DeleteBlob target _) ->
                      deleteBlob store target

-- NOTE(dbp 2018-12-15): When you want to delete a single blob,
-- sometimes others are logically connected. i.e., a file blob
-- should also take down the content blob(s).
findConnectedBlobs :: SomeBlobServer -> SHA224 -> IO [SHA224]
findConnectedBlobs store sha = do
  bs <- readBlob store sha
  let iffile = case decode =<< bs of
                 Just (FileBlob _ ps) -> map (\(Part r _) -> r) ps
                 Nothing -> []
  return (sha:iffile)
