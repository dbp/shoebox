{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Shed.Indexer where

import           Control.Applicative                  ((<|>))
import           Control.Monad                        (void)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Lazy                 as BL
import qualified Data.Map                             as M
import           Data.Monoid                          ((<>))
import           Data.Text                            (Text)
import           Data.Time.Clock
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField

import           Shed.BlobServer
import           Shed.Signing
import           Shed.Types

data Permanode = Permanode { pId        :: Int
                           , sha1       :: SHA1
                           , attributes :: M.Map Text Text
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

data Part = Part SHA1 Int deriving Show

data Blob = Bytes BL.ByteString
          | PermanodeBlob SHA1 Text
          | SetAttribute SHA1 UTCTime SHA1 Text Text
          | FileBlob Text [Part]
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
  parseJSON invalid = typeMismatch "SHA1" invalid

instance ToJSON Part where
  toJSON (Part ref size) = object ["blobRef" .= ref
                                  ,"size" .= size]

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
                               else fail "not a set-attribute")
                         <|>
                         (do t <- v .: "camliType"
                             if t == ("file" :: Text) then
                               FileBlob <$> v .: "fileName"
                                    <*> v .: "parts"
                               else fail "not a set-attribute")
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


blobToSignedJson :: Key -> Blob -> IO ByteString
blobToSignedJson k b = signJson k $ BL.toStrict $ encode b

indexBlob :: Connection -> SHA1 -> Blob -> IO ()
indexBlob conn (SHA1 sha) (PermanodeBlob _ _) =
  void $ execute conn "INSERT INTO permanodes (sha1) VALUES (?) ON CONFLICT DO NOTHING" (Only sha)
indexBlob conn (SHA1 sha) (SetAttribute s d p a v) =
  void $ execute conn "UPDATE permanodes SET attributes = jsonb_set(attributes, ARRAY[?], ?) WHERE sha1 = ?" (a,String v,p)
indexBlob conn (SHA1 sha) (FileBlob _ _) =
  void $ execute conn "UPDATE permanodes SET show_in_ui = true WHERE attributes->'camliContent' = ?" (Only (String sha))
indexBlob _ _ (Bytes _) = return ()

index :: BlobServer a => a -> IO ()
index a = do
  conn <- connectPostgreSQL "dbname=shed"
  putStrLn ""
  enumerateBlobs a $ \sha dat -> do
    putStr $ "\r" <> show sha
    let blob = case decode dat of
                 Nothing -> Bytes dat
                 Just b  -> b
    indexBlob conn sha blob
  putStrLn "\rDONE                                            "

wipe :: IO ()
wipe = do
  conn <- connectPostgreSQL "dbname=shed"
  void $ execute_ conn "delete from permanodes"

