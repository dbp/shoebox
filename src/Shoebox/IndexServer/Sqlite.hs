{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Shoebox.IndexServer.Sqlite where

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

import           Shoebox.IndexServer
import           Shoebox.Types

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
  wipe (SL conn) = do
    void $ execute_ conn "DELETE FROM items"
    void $ execute_ conn "DELETE FROM redirs"

  makeItem (SL conn) sha =
    void $ execute conn "INSERT OR IGNORE INTO items (blob_ref) VALUES (?)" (Only sha)

  removeItem (SL conn) sha =
    void $ execute conn "DELETE FROM items where blob_ref = ?" (Only sha)

  setSearchHigh (SL conn) (SHA224 sha) text =
    void $ execute conn "UPDATE items SET search_high = ? WHERE blob_ref = ?" (text, sha)

  setSearchLow (SL conn) (SHA224 sha) text =
    void $ execute conn "UPDATE items SET search_low = ? WHERE blob_ref = ?" (text, sha)

  setThumbnail (SL conn) (SHA224 sha) jpg =
     void $ execute conn "UPDATE items SET thumbnail = ? WHERE blob_ref = ?" (jpg, sha)

  setPreview (SL conn) (SHA224 sha) prev =
    void $ execute conn "UPDATE items SET preview = ? WHERE blob_ref = ?" (prev, sha)

  showInRoot (SL conn) sha =
    void $ execute conn "UPDATE items SET show_in_root = 1 where blob_ref = ?" (Only sha)

  getItem (SL conn) (SHA224 sha) = listToMaybe <$> query conn "SELECT blob_ref, thumbnail, preview FROM items WHERE blob_ref = ?" (Only sha)

  getItems (SL conn) page = query conn "SELECT blob_ref, thumbnail, preview FROM items WHERE show_in_root = 1 ORDER BY blob_ref DESC LIMIT 100 OFFSET ?" (Only (100 * page))

  search (SL conn) t = query conn "SELECT blob_ref, thumbnail, preview FROM items WHERE search_high LIKE ? OR search_low LIKE ?" ("%"<>t<>"%","%"<>t<>"%")

  getThumbnail (SL conn) (SHA224 sha) =
    do res <- listToMaybe <$> query conn "SELECT thumbnail FROM items WHERE blob_ref = ?" (Only sha)
       case res of
         Nothing         -> return Nothing
         Just (Only jpg) -> return (Just jpg)

  getRedirections (SL conn) from =
    fmap (map head) $ query conn "SELECT target FROM redirs WHERE src = ?" (Only from)

  setRedirection (SL conn) (SHA224 from) (SHA224 to) = do
    targets <- getRedirections (SL conn) (SHA224 to)
    case targets of
      [] -> do
        void $ execute conn "INSERT INTO redirs (src,target) SELECT ?,? WHERE NOT EXISTS (SELECT 1 FROM redirs WHERE src = ? AND target = ?);" (from, to, from, to)
        -- NOTE(dbp 2018-12-15): If we insert B -> C, any A -> B should be updated to A -> C.
        void $ execute conn "UPDATE redirs SET target = ? WHERE target = ?;" (to, from)
      ts -> do
        -- NOTE(dbp 2018-12-15): This means that we wanted A -> B, but B -> C1,C2,... already, so do A -> C1, A -> C2...
        mapM_ (\t -> do execute conn "INSERT INTO redirs (src,target) SELECT ?,? WHERE NOT EXISTS (SELECT 1 FROM redirs WHERE src = ? AND target = ?);" (from, t, from, t)
                        execute conn "UPDATE redirs SET target = ? WHERE target = ?;" (t, from)) ts

  getUrls (SL conn) url =
    fmap (map head) $ query conn "SELECT R.target FROM urls as U join (SELECT src,target FROM redirs UNION SELECT ref as src,ref as target FROM urls) as R on R.src = U.ref WHERE url = ?" (Only url)

  setUrl (SL conn) url ref url_ref =
    void $ execute conn "INSERT INTO urls (ref,url,url_blob_ref) SELECT ?,?,? WHERE NOT EXISTS (SELECT 1 FROM urls WHERE ref = ? AND url = ?);" (ref, url, url_ref, ref, url)

  getWithUrl (SL conn) ref =
    query conn "SELECT U.url_blob_ref, U.url FROM urls as U join (SELECT src,target FROM redirs UNION SELECT ref as src,ref as target FROM urls) as R on R.src = U.ref WHERE R.target = ?" (Only ref)

  removeUrl (SL conn) sha =
    void $ execute conn "DELETE FROM urls where url_blob_ref = ?" (Only sha)
