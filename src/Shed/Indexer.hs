{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Shed.Indexer where

import           Control.Applicative                  ((<|>))
import           Control.Monad                        (void, when)
import           Data.Aeson                           (decode)
import           Data.Aeson.Encode.Pretty             (encodePretty)
import           Data.Aeson.Types                     (FromJSON (..),
                                                       ToJSON (..), Value (..),
                                                       object, typeMismatch,
                                                       (.:), (.=))
import           Data.Binary.Builder                  (Builder)
import qualified Data.Binary.Builder                  as Builder
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Lazy                 as BL
import           Data.ByteString.Unsafe               (unsafeUseAsCStringLen)
import qualified Data.Map                             as M
import           Data.Maybe                           (catMaybes, listToMaybe)
import           Data.Monoid                          ((<>))
import           Data.Text                            (Text)
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField (FromField (..),
                                                       fromJSONField)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Magic                                (MagicFlag (MagicMimeType),
                                                       magicCString,
                                                       magicLoadDefault,
                                                       magicOpen)


import           Shed.BlobServer
import           Shed.Images
import           Shed.Signing
import           Shed.Types

data Permanode = Permanode { sha1       :: SHA1
                           , attributes :: M.Map Text Text
                           , thumbnail  :: Maybe ByteString
                           , preview    :: Maybe Text
                           }

instance FromField SHA1 where
  fromField f b = SHA1 <$> fromField f b

instance ToField SHA1 where
  toField (SHA1 a) = toField a

instance FromField (M.Map Text Text) where
  fromField = fromJSONField

instance FromRow Permanode where
  fromRow = Permanode <$> field
                      <*> field
                      <*> field
                      <*> field

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


indexBlob :: BlobServer a => a -> Connection -> SHA1 -> Blob -> IO ()
indexBlob store conn (SHA1 sha) (PermanodeBlob _ _) =
  void $ execute conn "INSERT INTO permanodes (sha1) VALUES (?) ON CONFLICT DO NOTHING" (Only sha)
indexBlob store conn (SHA1 sha) (SetAttribute s d p a v) =
  void $ execute conn "UPDATE permanodes SET attributes = jsonb_set(attributes, ARRAY[?], ?) WHERE sha1 = ?" (a,String v,p)
indexBlob store conn (SHA1 sha) (FileBlob _ parts) = do
  Just (Only n) <- listToMaybe <$> query conn "SELECT COUNT(*) FROM permanodes WHERE attributes->'camliContent' = ?" (Only (String sha))
  when (n > (0 :: Int)) $ do
    execute conn "UPDATE permanodes SET show_in_ui = true WHERE attributes->'camliContent' = ?" (Only (String sha))
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
                  Nothing -> return ()
                  Just jpg -> void $ execute conn "UPDATE permanodes SET thumbnail = ? WHERE attributes->'camliContent' = ?" (Binary jpg, String sha)
           case mime of
             "image/jpeg" -> mkThumb
             "image/png"  -> mkThumb
      Just jpg -> void $ execute conn "UPDATE permanodes SET thumbnail = ? WHERE attributes->'camliContent' = ?" (Binary jpg, String sha)
indexBlob store conn (SHA1 sha) (EmailBlob from headers body) = do
  Just (Only n) <- listToMaybe <$> query conn "SELECT COUNT(*) FROM permanodes WHERE attributes->'camliContent' = ?" (Only (String sha))
  when (n > (0 :: Int)) $ do
    execute conn "UPDATE permanodes SET show_in_ui = true WHERE attributes->'camliContent' = ?" (Only (String sha))
    let preview = (maybe "" (<> "\n") (getHeader headers "Subject"))
               <> (maybe "" (\x -> "From: " <> x <> "\n") (getHeader headers "From"))
               <> (maybe "" (\x -> "Date: " <> x <> "\n") (getHeader headers "Date"))
    void $ execute conn "UPDATE permanodes SET preview = ? WHERE attributes->'camliContent' = ?" (preview, String sha)
indexBlob _ _ _ (Bytes _) = return ()

index :: BlobServer a => a -> IO ()
index a = do
  conn <- connectPostgreSQL "dbname=shed"
  putStrLn ""
  enumerateBlobs a $ \sha dat -> do
    putStr $ "\r" <> show sha
    let blob = case decode dat of
                 Nothing -> Bytes dat
                 Just b  -> b
    indexBlob a conn sha blob
  putStrLn "\rDONE                                            "

wipe :: IO ()
wipe = do
  conn <- connectPostgreSQL "dbname=shed"
  void $ execute_ conn "delete from permanodes"

