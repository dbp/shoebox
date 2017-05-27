{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Shed.Indexer where

import           Control.Applicative                  ((<|>))
import           Control.Monad                        (void)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Lazy                 as BL
import qualified Data.Map                             as M
import           Data.Text                            (Text)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.Shed.BlobServer
import           Database.Shed.Types

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
          | PermanodeBlob
          | SetAttribute SHA1 Text Text
          | FileBlob Text [Part]
          deriving Show

instance FromJSON SHA1 where
  parseJSON (String s) = return (SHA1 s)
  parseJSON invalid    = typeMismatch "SHA1" invalid

instance FromJSON Part where
  parseJSON (Object v) = Part <$> v .: "blobRef"
                              <*> v .: "size"
  parseJSON invalid = typeMismatch "SHA1" invalid

instance FromJSON Blob where
  parseJSON (Object v) = (do t <- v .: "camliType"
                             if t == ("permanode" :: Text)
                               then return PermanodeBlob
                               else fail "Not a permanode")
                         <|>
                         (do t <- v .: "claimType"
                             if t == ("set-attribute" :: Text) then
                               SetAttribute <$> v .: "permaNode"
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

index :: BlobServer a => a -> IO ()
index a = do
  conn <- connectPostgreSQL "dbname=shed"
  enumerateBlobs a $ \sha dat -> do
    let blob = case decode dat of
                 Nothing -> Bytes dat
                 Just b  -> b
    void $ case blob of
             PermanodeBlob ->
               execute conn "INSERT INTO permanodes (sha1) VALUES (?) ON CONFLICT DO NOTHING" (Only sha)
             SetAttribute s a v ->
               execute conn "UPDATE permanodes SET attributes = jsonb_set(attributes, ARRAY[?], ?) WHERE sha1 = ?" (a,String v,s)
             FileBlob _ _ ->
               execute conn "UPDATE permanodes SET show_in_ui = true WHERE attributes->'camliContent' = ?" (Only (String (unSHA1 sha)))
             Bytes _ -> return 0
