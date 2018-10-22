{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Shed.IndexServer.Sqlite where

import           Control.Monad
import           Data.Aeson                       (decode, encode)
import           Data.Aeson.Types                 (Value (..))
import qualified Data.ByteString.Lazy             as BL
import qualified Data.Map                         as M
import           Data.Maybe                       (listToMaybe)
import           Data.Monoid                      ((<>))
import           Data.Text                        (Text)
import qualified Data.Text.Encoding               as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField (FromField (..))
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.Ok        (Ok (..))
import           Database.SQLite.Simple.ToField

import           Shed.IndexServer
import           Shed.Types

newtype SqliteIndexer = SL { unSqliteIndexer :: Connection }

instance FromField SHA224 where
  fromField f = SHA224 <$> fromField f

instance ToField SHA224 where
  toField (SHA224 a) = toField a

instance FromField (M.Map Text Text) where
  fromField f = case fromField f of
                  Ok s -> case decode (BL.fromStrict $ T.encodeUtf8 s) of
                            Just v  -> Ok v
                            Nothing -> Errors []
                  Errors es -> Errors es

instance FromRow Item where
  fromRow = Item <$> field
                 <*> field
                 <*> field


instance ToField Value where
  toField v = toField (encode v)

instance IndexServer SqliteIndexer where
  wipe (SL conn) = void $ execute_ conn "DELETE FROM items"

  makeItem (SL conn) sha =
    void $ execute conn "INSERT OR IGNORE INTO items (blob_ref) VALUES (?)" (Only sha)

  setSearchHigh (SL conn) (SHA224 sha) text =
    void $ execute conn "UPDATE items SET search_high = ? WHERE blob_ref = ?" (text, sha)

  setSearchLow (SL conn) (SHA224 sha) text =
    void $ execute conn "UPDATE items SET search_low = ? WHERE blob_ref = ?" (text, sha)

  setThumbnail (SL conn) (SHA224 sha) jpg =
     void $ execute conn "UPDATE items SET thumbnail = ? WHERE blob_ref = ?" (jpg, sha)

  setPreview (SL conn) (SHA224 sha) prev =
    void $ execute conn "UPDATE items SET preview = ? WHERE blob_ref = ?" (prev, sha)

  getItem (SL conn) (SHA224 sha) = listToMaybe <$> query conn "SELECT blob_ref, thumbnail, preview FROM items WHERE blob_ref = ?" (Only sha)

  getItems (SL conn) page = query conn "SELECT blob_ref, thumbnail, preview FROM items ORDER BY blob_ref DESC LIMIT 100 OFFSET ?" (Only (100 * page))

  search (SL conn) t = query conn "SELECT blob_ref, thumbnail, preview FROM items WHERE search_high LIKE ? OR search_low LIKE ?" ("%"<>t<>"%","%"<>t<>"%")

  getThumbnail (SL conn) (SHA224 sha) =
    do res <- listToMaybe <$> query conn "SELECT thumbnail FROM items WHERE blob_ref = ?" (Only sha)
       case res of
         Nothing         -> return Nothing
         Just (Only jpg) -> return (Just jpg)
