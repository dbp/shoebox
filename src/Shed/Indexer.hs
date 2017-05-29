{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Shed.Indexer where

import           Control.Applicative      ((<|>))
import           Control.Logging          (log')
import           Control.Monad            (void, when)
import           Data.Aeson               (decode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson.Types         (FromJSON (..), ToJSON (..),
                                           Value (..), object, typeMismatch,
                                           (.:), (.=))
import           Data.Binary.Builder      (Builder)
import qualified Data.Binary.Builder      as Builder
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Data.ByteString.Unsafe   (unsafeUseAsCStringLen)
import qualified Data.Map                 as M
import           Data.Maybe               (catMaybes, fromMaybe, listToMaybe)
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Data.Time.Clock
import           Magic                    (MagicFlag (MagicMimeType),
                                           magicCString, magicLoadDefault,
                                           magicOpen)


import           Shed.BlobServer
import           Shed.Images
import           Shed.IndexServer
import           Shed.Signing
import           Shed.Types

data Part = Part SHA1 Int deriving Show

data Header = Header Text Text deriving Show

getHeader :: [Header] -> Text -> Maybe Text
getHeader hs k = (\(Header _ v) -> v) <$> listToMaybe (filter (\(Header k' _) -> k == k') hs)

data Blob = Bytes BL.ByteString
          | PermanodeBlob SHA1 Text
          | SetAttribute SHA1 UTCTime SHA1 Text Text
          | FileBlob Text [Part]
          | EmailBlob Text [Header] [Part]
          deriving Show

readBlob' :: BlobServer a => a -> SHA1 -> IO (Maybe Blob)
readBlob' a s = do b <- readBlob a s
                   case b of
                     Nothing -> return Nothing
                     Just res -> case decode res of
                                   Nothing   -> return (Just $ Bytes res)
                                   Just blob -> return blob

instance FromJSON SHA1 where
  parseJSON (String s) = return (SHA1 s)
  parseJSON invalid    = typeMismatch "SHA1" invalid

instance ToJSON SHA1 where
  toJSON (SHA1 sha) = String sha

instance FromJSON Part where
  parseJSON (Object v) = Part <$> v .: "blobRef"
                              <*> v .: "size"
  parseJSON invalid = typeMismatch "Part" invalid

instance ToJSON Part where
  toJSON (Part ref size) = object ["blobRef" .= ref
                                  ,"size" .= size]

instance FromJSON Header where
  parseJSON (Object v) = Header <$> v .: "name"
                                <*> v .: "value"
  parseJSON invalid = typeMismatch "Header" invalid

instance ToJSON Header where
  toJSON (Header name value) = object ["name" .= name
                                      ,"value" .= value]

instance FromJSON Blob where
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
                         <|>
                         (do t <- v .: "camliType"
                             if t == ("file" :: Text) then
                               FileBlob <$> v .: "fileName"
                                        <*> v .: "parts"
                               else fail "Not a file")
                         <|>
                         (do t <- v .: "camliType"
                             if t == ("email" :: Text) then
                               EmailBlob <$> v .: "from"
                                         <*> v .: "headers"
                                         <*> v .: "body"
                             else fail "Not an email")
  parseJSON invalid    = typeMismatch "Blob" invalid


instance ToJSON Blob where
  toJSON (PermanodeBlob signer random) =
    object ["camliVersion" .= (1 :: Int)
           ,"camliType" .= ("permanode" :: Text)
           ,"camliSigner" .= signer
           ,"random" .= random]
  toJSON (Bytes _) = error "Cannot encode Bytes as JSON"
  toJSON (SetAttribute signer date permanode attr val) =
    object ["camliVersion" .= (1 :: Int)
           ,"camliType" .= ("claim" :: Text)
           ,"camliSigner" .= signer
           ,"claimDate" .= date
           ,"claimType" .= ("set-attribute" :: Text)
           ,"attribute" .= attr
           ,"value" .= val]
  toJSON (FileBlob name parts) =
    object ["camliVersion" .= (1 :: Int)
           ,"camliType" .= ("file" :: Text)
           ,"fileName" .= name
           ,"parts" .= parts]
  toJSON (EmailBlob from headers body) =
    object ["camliVersion" .= (1 :: Int)
           ,"camliType" .= ("email" :: Text)
           ,"from" .= from
           ,"headers" .= headers
           ,"body" .= body]


blobToSignedJson :: Key -> Blob -> IO ByteString
blobToSignedJson k b = signJson k $ BL.toStrict $ encodePretty b

readFileBytes :: BlobServer a => a -> [Part] -> IO Builder
readFileBytes store ps =
  do bs <- catMaybes <$> mapM (\(Part sha _) -> readBlob store sha) ps
     return $ foldl (\builder st ->
               Builder.append builder
               (Builder.fromLazyByteString st))
       Builder.empty
       bs


indexBlob :: BlobServer a => a -> AnIndexServer -> SHA1 -> Blob -> IO ()
indexBlob store (AnIndexServer serv) sha (PermanodeBlob _ _) =
  makePermanode serv sha
indexBlob store (AnIndexServer serv) sha (SetAttribute s d p a v) =
  setPermanodeAttribute serv p a v
indexBlob store (AnIndexServer serv) sha (FileBlob name parts) = do
  exists <- permanodeHasContent serv sha
  when exists $ do
    setPermanodeShowInUI serv sha
    setSearchHigh serv sha name
    builder <- readFileBytes store parts
    let dat = BL.toStrict $ Builder.toLazyByteString builder
    res <- getExifThumbnail dat
    case res of
      Nothing  ->
        do m <- magicOpen [MagicMimeType]
           magicLoadDefault m
           mime <- unsafeUseAsCStringLen dat (magicCString m)
           let mkThumb = do
                thm <- createThumbnail dat
                case thm of
                  Nothing  -> return ()
                  Just jpg -> setPermanodeThumbnail serv sha (BL.toStrict jpg)
           case mime of
             "image/jpeg" -> mkThumb
             "image/png"  -> mkThumb
      Just jpg -> setPermanodeThumbnail serv sha jpg
indexBlob store (AnIndexServer serv) sha (EmailBlob from headers body) = do
  exists <- permanodeHasContent serv sha
  when exists $ do
    setPermanodeShowInUI serv sha
    setSearchHigh serv sha $
      T.concat [fromMaybe "" $ getHeader headers "From", "\n"
               ,fromMaybe "" $ getHeader headers "Subject", "\n"]
    b <- readFileBytes store body
    let body' = T.decodeUtf8 $ BL.toStrict $ Builder.toLazyByteString b
    setSearchLow serv sha body'
    let preview = (maybe "" (<> "\n") (getHeader headers "Subject"))
               <> (maybe "" (\x -> "From: " <> x <> "\n") (getHeader headers "From"))
               <> (maybe "" (\x -> "Date: " <> x <> "\n") (getHeader headers "Date"))
    setPermanodePreview serv sha preview
indexBlob _ _ _ (Bytes _) = return ()

index :: BlobServer a => a -> AnIndexServer -> IO ()
index a s = do
  putStrLn ""
  enumerateBlobs a $ \sha dat -> do
    putStr $ "\r" <> show sha
    let blob = case decode dat of
                 Nothing -> Bytes dat
                 Just b  -> b
    indexBlob a s sha blob
  putStrLn "\rDONE                                            "
