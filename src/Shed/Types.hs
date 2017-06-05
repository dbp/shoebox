{-# LANGUAGE OverloadedStrings #-}
module Shed.Types where

import           Data.Aeson.Types (FromJSON (..), ToJSON (..), Value (..),
                                   typeMismatch)
import           Data.ByteString  (ByteString)
import qualified Data.Map         as M
import           Data.Text        (Text)
import qualified Data.Text        as T

newtype SHA1 = SHA1 { unSHA1 :: Text }
instance Show SHA1 where
  show (SHA1 s) = T.unpack s

data Key = Key { keyId :: Text, keyBlobRef :: SHA1 } deriving Show

instance FromJSON SHA1 where
  parseJSON (String s) = return (SHA1 s)
  parseJSON invalid    = typeMismatch "SHA1" invalid

instance ToJSON SHA1 where
  toJSON (SHA1 sha) = String sha

data Permanode = Permanode { sha1       :: SHA1
                           , attributes :: M.Map Text Text
                           , thumbnail  :: Maybe ByteString
                           , preview    :: Maybe Text
                           } deriving Show
