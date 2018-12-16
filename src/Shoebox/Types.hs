{-# LANGUAGE OverloadedStrings #-}
module Shoebox.Types where

import           Data.Aeson.Types (FromJSON (..), ToJSON (..), Value (..),
                                   typeMismatch)
import           Data.ByteString  (ByteString)
import qualified Data.Map         as M
import           Data.Text        (Text)
import qualified Data.Text        as T

newtype SHA224 = SHA224 { unSHA224 :: Text } deriving (Eq, Ord)
instance Show SHA224 where
  show (SHA224 s) = T.unpack s

instance FromJSON SHA224 where
  parseJSON (String s) = return (SHA224 s)
  parseJSON invalid    = typeMismatch "SHA224" invalid

instance ToJSON SHA224 where
  toJSON (SHA224 sha) = String sha

data Item = Item { blob_ref  :: SHA224
                 , thumbnail :: Maybe ByteString
                 , preview   :: Maybe Text
                 } deriving Show
