{-# LANGUAGE OverloadedStrings #-}
module Shoebox.Types where

import qualified Crypto.Hash             as Hash
import           Crypto.Random           (getRandomBytes)
import           Data.Aeson.Types        (FromJSON (..), ToJSON (..),
                                          Value (..), typeMismatch)
import           Data.ByteArray          (convert)
import           Data.ByteString         (ByteString)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Map                as M
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T


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

mkRandom :: IO Text
mkRandom = do bytes <- getRandomBytes 32 :: IO ByteString
              let digest = Hash.hash bytes :: Hash.Digest Hash.SHA1
              return $ T.decodeUtf8 $ BL.toStrict $ Builder.toLazyByteString $ Builder.byteStringHex (convert digest)
