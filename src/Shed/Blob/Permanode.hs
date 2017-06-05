{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
-- This module contains logic pertaining to camliType "permanode"
module Shed.Blob.Permanode where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson.Types
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Lazy     as BL
import           Data.Maybe               (isJust)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Data.Time.Clock
import           Network.Wai              (Response)
import           System.Random            (randomIO)
import           Web.Fn                   (File (..))
import qualified Web.Larceny              as L

import           Shed.BlobServer
import           Shed.IndexServer
import           Shed.Signing
import           Shed.Types
import           Shed.Util

data PermanodeBlob = PermanodeBlob SHA1 Text
                   | SetAttribute SHA1 UTCTime SHA1 Text Text

instance FromJSON PermanodeBlob where
  parseJSON (Object v) = (do t <- v .: "camliType"
                             r <- v .: "random"
                             s <- v .: "camliSigner"
                             if t == ("permanode" :: Text)
                               then return (PermanodeBlob s r)
                               else fail "Not a permanode")
                         <|>
                         (do t <- v .: "claimType"
                             if t == ("set-attribute" :: Text) then
                               SetAttribute <$> v .: "camliSigner"
                                            <*> v .: "claimDate"
                                            <*> v .: "permaNode"
                                            <*> v .: "attribute"
                                            <*> v .: "value"
                               else fail "Not a set-attribute")
  parseJSON invalid    = typeMismatch "PermanodeBlob" invalid

instance ToJSON PermanodeBlob where
  toJSON (PermanodeBlob signer random) =
    object ["camliVersion" .= (1 :: Int)
           ,"camliType" .= ("permanode" :: Text)
           ,"camliSigner" .= signer
           ,"random" .= random]
  toJSON (SetAttribute signer date permanode attr val) =
    object ["camliVersion" .= (1 :: Int)
           ,"camliType" .= ("claim" :: Text)
           ,"camliSigner" .= signer
           ,"claimDate" .= date
           ,"claimType" .= ("set-attribute" :: Text)
           ,"permaNode" .= permanode
           ,"attribute" .= attr
           ,"value" .= val]

blobToSignedJson :: ToJSON a => Key -> a -> IO ByteString
blobToSignedJson k b = signJson k $ BL.toStrict $ encodePretty b

addPermanode :: SomeBlobServer -> Key -> IO (SHA1, PermanodeBlob)
addPermanode store key = do
  random <- T.pack . show . abs <$> (randomIO :: IO Int)
  let permablob = PermanodeBlob (keyBlobRef key) random
  perma <- blobToSignedJson key permablob
  (, permablob) <$> writeBlob store perma

setAttribute :: SomeBlobServer -> Key -> SHA1 -> Text -> Text -> IO (SHA1, PermanodeBlob)
setAttribute store key pref name value = do
  now <- getCurrentTime
  let claimblob = SetAttribute (keyBlobRef key) now pref name value
  claim <- blobToSignedJson key claimblob
  (, claimblob) <$> writeBlob store claim


indexBlob :: SomeBlobServer -> SomeIndexServer -> SHA1 -> PermanodeBlob -> IO ()
indexBlob store serv sha (PermanodeBlob _ _) =
  makePermanode serv sha
indexBlob store serv sha (SetAttribute s d p a v) =
  setPermanodeAttribute serv p a v

recognizeBlob :: SomeBlobServer -> SomeIndexServer -> Key -> File -> (File -> IO ()) -> IO ()
recognizeBlob _ _ _ _ _ = return ()

toHtml :: SomeBlobServer -> SomeIndexServer -> (L.Substitutions () -> Text -> IO (Maybe Response)) -> SHA1 -> BL.ByteString -> IO (Maybe Response)
toHtml store serv renderWith sha bs =
  case decode bs of
    Just (PermanodeBlob _ _) -> do
      mp <- getPermanode serv sha
      let ex =  case mp of
                  Nothing -> Nothing
                  Just p  -> Just $ BL.toStrict $ encodePretty (toJSON $ attributes p)
      renderWith (L.subs [("content", L.rawTextFill (hyperLinkEscape (T.decodeUtf8 $ BL.toStrict bs)))
                         ,("extra", if isJust ex then
                               L.fillChildrenWith (L.subs [("content", L.rawTextFill (hyperLinkEscape $ maybe "" T.decodeUtf8 ex))]) else L.textFill "")])
           "blob"
    Just _ -> rawBlob bs
    _ -> return Nothing
