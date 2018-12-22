{-# LANGUAGE OverloadedStrings #-}
module Shoebox.Blob.Note where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import           Data.Time.Clock

import           Shoebox.BlobServer
import           Shoebox.IndexServer
import           Shoebox.Types

data NoteBlob = NoteBlob { ref     :: SHA224
                         , content :: Text
                         , random  :: Text
                         }

instance FromJSON NoteBlob where
  parseJSON (Object v) = (do t <- v .: "type"
                             if t == ("note" :: Text) then
                               NoteBlob <$> v .: "ref"
                                        <*> v .: "content"
                                        <*> v .: "random"
                               else fail "Not a note")
  parseJSON invalid    = typeMismatch "NoteBlob" invalid

instance ToJSON NoteBlob where
  toJSON (NoteBlob ref content random) = object ["version" .= (1 :: Int)
                                                ,"type" .= ("note" :: Text)
                                                ,"ref" .= ref
                                                ,"content" .= content
                                                ,"random" .= random]

indexBlob :: SomeBlobServer -> SomeIndexServer -> SHA224 -> NoteBlob -> IO ()
indexBlob store serv sha (NoteBlob ref content _) = do
  replaced <- not . null <$> getRedirections serv sha
  if replaced then return () else setNote serv content ref sha
