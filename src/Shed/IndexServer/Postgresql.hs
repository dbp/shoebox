{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Shed.IndexServer.Postgresql where

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

import           Shed.IndexServer
import           Shed.Types

newtype PostgresIndexer = PG { unPostgresIndexer :: Connection }

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


instance IndexServer PostgresIndexer where
  wipe (PG conn) = void $ execute_ conn "delete from permanodes"

  makePermanode (PG conn) sha =
    void $ execute conn "INSERT INTO permanodes (sha1) VALUES (?) ON CONFLICT DO NOTHING" (Only sha)

  setPermanodeAttribute (PG conn) sha k v =
    void $ execute conn "UPDATE permanodes SET attributes = jsonb_set(attributes, ARRAY[?], ?) WHERE sha1 = ?" (k,String v,sha)

  permanodeHasContent (PG conn) (SHA1 sha) =
    do Just (Only n) <- listToMaybe <$> query conn "SELECT COUNT(*) FROM permanodes WHERE attributes->'camliContent' = ?" (Only (String sha))
       return (n > (0 :: Int))

  setPermanodeShowInUI (PG conn) (SHA1 sha) =
    void $ execute conn "UPDATE permanodes SET show_in_ui = true WHERE attributes->'camliContent' = ?" (Only (String sha))

  setSearchHigh (PG conn) (SHA1 sha) text =
    void $ execute conn "UPDATE permanodes SET search_high = ? WHERE attributes->'camliContent' = ?" (text, String sha)

  setSearchLow (PG conn) (SHA1 sha) text =
    void $ execute conn "UPDATE permanodes SET search_low = ? WHERE attributes->'camliContent' = ?" (text, String sha)

  setPermanodeThumbnail (PG conn) (SHA1 sha) jpg =
     void $ execute conn "UPDATE permanodes SET thumbnail = ? WHERE attributes->'camliContent' = ?" (Binary jpg, String sha)

  setPermanodePreview (PG conn) (SHA1 sha) prev =
    void $ execute conn "UPDATE permanodes SET preview = ? WHERE attributes->'camliContent' = ?" (prev, String sha)

  getPermanode (PG conn) (SHA1 sha) = listToMaybe <$> query conn "SELECT sha1, attributes, thumbnail, preview FROM permanodes WHERE sha1 = ?" (Only sha)

  getPermanodes (PG conn) page = query conn "SELECT sha1, attributes, thumbnail, preview FROM permanodes WHERE show_in_ui = true ORDER BY sha1 DESC LIMIT 100 OFFSET ?" (Only (100 * page))

  search (PG conn) t = query conn "SELECT sha1, attributes, thumbnail, preview FROM permanodes WHERE (setweight(to_tsvector(permanodes.search_high),'A') || setweight(to_tsvector(permanodes.search_low), 'B')) @@ to_tsquery('english', ?) ORDER BY ts_rank((setweight(to_tsvector(permanodes.search_high),'A') || setweight(to_tsvector(permanodes.search_low), 'B')), to_tsquery('english', ?)) DESC" (t,t)

  getThumbnail (PG conn) (SHA1 sha) =
    do res <- listToMaybe <$> query conn "SELECT thumbnail FROM permanodes WHERE sha1 = ?" (Only sha)
       case res of
         Nothing                  -> return Nothing
         Just (Only (Binary jpg)) -> return (Just jpg)
