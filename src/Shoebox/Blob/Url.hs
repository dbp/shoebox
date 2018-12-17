{-# LANGUAGE OverloadedStrings #-}
module Shoebox.Blob.Url where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import           Data.Time.Clock

import           Shoebox.BlobServer
import           Shoebox.IndexServer
import           Shoebox.Types

data UrlBlob = UrlBlob { url    :: Text
                       , ref    :: SHA224
                       , random :: Text
                       }

instance FromJSON UrlBlob where
  parseJSON (Object v) = (do t <- v .: "type"
                             if t == ("url" :: Text) then
                               UrlBlob <$> v .: "url"
                                       <*> v .: "ref"
                                       <*> v .: "random"
                               else fail "Not a url")
  parseJSON invalid    = typeMismatch "UrlBlob" invalid

instance ToJSON UrlBlob where
  toJSON (UrlBlob url ref random) = object ["version" .= (1 :: Int)
                                           ,"type" .= ("url" :: Text)
                                           ,"url" .= url
                                           ,"ref" .= ref
                                           ,"random" .= random]

indexBlob :: SomeBlobServer -> SomeIndexServer -> SHA224 -> UrlBlob -> IO ()
indexBlob store serv sha (UrlBlob url ref _) = do
  setUrl serv url ref sha
