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
  wipe (PG conn) = void $ execute_ conn "delete from items"

  makeItem (PG conn) sha =
    void $ execute conn "INSERT INTO items (blob_ref) VALUES (?) ON CONFLICT DO NOTHING" (Only sha)

  removeItem (PG conn) sha =
    void $ execute conn "DELETE FROM items where blob_ref = ?" (Only sha)

  setSearchHigh (PG conn) (SHA224 sha) text =
    void $ execute conn "UPDATE items SET search_high = ? WHERE attributes->'camliContent' = ?" (text, String sha)

  setSearchLow (PG conn) (SHA224 sha) text =
    void $ execute conn "UPDATE items SET search_low = ? WHERE attributes->'camliContent' = ?" (text, String sha)

  setThumbnail (PG conn) (SHA224 sha) jpg =
     void $ execute conn "UPDATE items SET thumbnail = ? WHERE attributes->'camliContent' = ?" (Binary jpg, String sha)

  setPreview (PG conn) (SHA224 sha) prev =
    void $ execute conn "UPDATE items SET preview = ? WHERE attributes->'camliContent' = ?" (prev, String sha)

  getItem (PG conn) (SHA224 sha) = listToMaybe <$> query conn "SELECT blob_ref, thumbnail, preview FROM items WHERE blob_ref = ?" (Only sha)

  getItems (PG conn) page = query conn "SELECT blob_ref,  thumbnail, preview FROM items WHERE show_in_ui = true ORDER BY blob_ref DESC LIMIT 100 OFFSET ?" (Only (100 * page))

  search (PG conn) t = query conn "SELECT blob_ref, thumbnail, preview FROM items WHERE (setweight(to_tsvector(items.search_high),'A') || setweight(to_tsvector(items.search_low), 'B')) @@ to_tsquery('english', ?) ORDER BY ts_rank((setweight(to_tsvector(items.search_high),'A') || setweight(to_tsvector(items.search_low), 'B')), to_tsquery('english', ?)) DESC" (t,t)

  getThumbnail (PG conn) (SHA224 sha) =
    do res <- listToMaybe <$> query conn "SELECT thumbnail FROM items WHERE blob_ref = ?" (Only sha)
       case res of
         Nothing                  -> return Nothing
         Just (Only (Binary jpg)) -> return (Just jpg)
  getRedirections (PG conn) from = error "Not implemented"
  setRedirection (PG conn) from to = error "Not implemented"
