{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Shoebox.IndexServer.Postgresql where

import           Control.Monad
import           Data.Aeson.Types                     (Value (..))
import qualified Data.Map                             as M
import           Data.Maybe                           (listToMaybe)
import           Data.Text                            (Text)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField (FromField (..),
                                                       fromJSONField)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField

import           Shoebox.IndexServer
import           Shoebox.Types

newtype PostgresIndexer = PG { unPostgresIndexer :: Connection }

instance FromField SHA224 where
  fromField f b = SHA224 <$> fromField f b

instance ToField SHA224 where
  toField (SHA224 a) = toField a

instance FromField (M.Map Text Text) where
  fromField = fromJSONField

instance FromRow Item where
  fromRow = Item <$> field
                 <*> field
                 <*> field


instance IndexServer PostgresIndexer where
  wipe (PG conn) = do
    void $ execute_ conn "DELETE FROM items"
    void $ execute_ conn "DELETE FROM redirs"

  makeItem (PG conn) sha =
    void $ execute conn "INSERT INTO items (blob_ref) VALUES (?) ON CONFLICT DO NOTHING" (Only sha)

  removeItem (PG conn) sha =
    void $ execute conn "DELETE FROM items where blob_ref = ?" (Only sha)

  setSearchHigh (PG conn) sha text =
    void $ execute conn "UPDATE items SET search_high = ? WHERE blob_ref = ?" (text, sha)

  setSearchLow (PG conn) sha text =
    void $ execute conn "UPDATE items SET search_low = ? WHERE blob_ref = ?" (text, sha)

  setThumbnail (PG conn) sha jpg =
     void $ execute conn "UPDATE items SET thumbnail = ? WHERE blob_ref = ?" (Binary jpg, sha)

  setPreview (PG conn) sha prev =
    void $ execute conn "UPDATE items SET preview = ? WHERE blob_ref = ?" (prev, sha)

  showInRoot (PG conn) sha =
    void $ execute conn "UPDATE items SET show_in_root = true where blob_ref = ?" (Only sha)

  getItem (PG conn) (SHA224 sha) = listToMaybe <$> query conn "SELECT blob_ref, thumbnail, preview FROM items WHERE blob_ref = ?" (Only sha)

  getItems (PG conn) page = query conn "SELECT blob_ref,  thumbnail, preview FROM items WHERE show_in_root = true ORDER BY blob_ref DESC LIMIT 100 OFFSET ?" (Only (100 * page))

  search (PG conn) t = query conn "SELECT blob_ref, thumbnail, preview FROM items WHERE (setweight(to_tsvector(items.search_high),'A') || setweight(to_tsvector(items.search_low), 'B')) @@ to_tsquery('english', ?) ORDER BY ts_rank((setweight(to_tsvector(items.search_high),'A') || setweight(to_tsvector(items.search_low), 'B')), to_tsquery('english', ?)) DESC" (t,t)

  getThumbnail (PG conn) (SHA224 sha) =
    do res <- listToMaybe <$> query conn "SELECT thumbnail FROM items WHERE blob_ref = ?" (Only sha)
       case res of
         Nothing                         -> return Nothing
         Just (Only Nothing)             -> return Nothing
         Just (Only (Just (Binary jpg))) -> return (Just jpg)

  getRedirections (PG conn) from =
    fmap (map head) $ query conn "SELECT target FROM redirs WHERE src = ?" (Only from)
  setRedirection (PG conn) from to =  do
    targets <- getRedirections (PG conn) to
    case targets of
      [] -> do
        void $ execute conn "INSERT INTO redirs (src,target) SELECT ?,? WHERE NOT EXISTS (SELECT 1 FROM redirs WHERE src = ? AND target = ?);" (from, to, from, to)
        -- NOTE(dbp 2018-12-15): If we insert B -> C, any A -> B should be updated to A -> C.
        void $ execute conn "UPDATE redirs SET target = ? WHERE target = ?;" (to, from)
      ts -> do
        -- NOTE(dbp 2018-12-15): This means that we wanted A -> B, but B -> C1,C2,... already, so do A -> C1, A -> C2...
        mapM_ (\t -> do execute conn "INSERT INTO redirs (src,target) SELECT ?,? WHERE NOT EXISTS (SELECT 1 FROM redirs WHERE src = ? AND target = ?);" (from, t, from, t)
                        execute conn "UPDATE redirs SET target = ? WHERE target = ?;" (t, from)) ts
