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

instance FromField SHA1 where
  fromField f = SHA1 <$> fromField f

instance ToField SHA1 where
  toField (SHA1 a) = toField a

instance FromField (M.Map Text Text) where
  fromField f = case fromField f of
                  Ok s -> case decode (BL.fromStrict $ T.encodeUtf8 s) of
                            Just v  -> Ok v
                            Nothing -> Errors []
                  Errors es -> Errors es

instance FromRow Permanode where
  fromRow = Permanode <$> field
                      <*> field
                      <*> field
                      <*> field


instance ToField Value where
  toField v = toField (encode v)

instance IndexServer SqliteIndexer where
  wipe (SL conn) = void $ execute_ conn "DELETE FROM permanodes"

  makePermanode (SL conn) sha =
    void $ execute conn "INSERT OR IGNORE INTO permanodes (sha1) VALUES (?)" (Only sha)

  setPermanodeAttribute (SL conn) sha k v = do
    mn <- getPermanode (SL conn) sha
    case mn of
      Nothing -> return ()
      Just node -> do
        let attrs = M.insert k v (attributes node)
        execute conn "UPDATE permanodes SET attributes = ? WHERE sha1 = ?" (T.decodeUtf8 $ BL.toStrict $ encode attrs, sha)
        when (k == "camliContent") $
          void $ execute conn "UPDATE permanodes SET content = ? WHERE sha1 = ?" (v, sha)

  permanodeHasContent (SL conn) (SHA1 sha) =
    do Just (Only n) <- listToMaybe <$> query conn "SELECT COUNT(*) FROM permanodes WHERE content = ?" (Only sha)
       return (n > (0 :: Int))

  setPermanodeShowInUI (SL conn) (SHA1 sha) =
    void $ execute conn "UPDATE permanodes SET show_in_ui = 1 WHERE content = ?" (Only sha)

  setSearchHigh (SL conn) (SHA1 sha) text =
    void $ execute conn "UPDATE permanodes SET search_high = ? WHERE content = ?" (text, sha)

  setSearchLow (SL conn) (SHA1 sha) text =
    void $ execute conn "UPDATE permanodes SET search_low = ? WHERE content = ?" (text, sha)

  setPermanodeThumbnail (SL conn) (SHA1 sha) jpg =
     void $ execute conn "UPDATE permanodes SET thumbnail = ? WHERE content = ?" (jpg, sha)

  setPermanodePreview (SL conn) (SHA1 sha) prev =
    void $ execute conn "UPDATE permanodes SET preview = ? WHERE content = ?" (prev, sha)

  getPermanode (SL conn) (SHA1 sha) = listToMaybe <$> query conn "SELECT sha1, attributes, thumbnail, preview FROM permanodes WHERE sha1 = ?" (Only sha)

  getPermanodes (SL conn) page = query conn "SELECT sha1, attributes, thumbnail, preview FROM permanodes WHERE show_in_ui = 1 ORDER BY sha1 DESC LIMIT 100 OFFSET ?" (Only (100 * page))

  search (SL conn) t = query conn "SELECT sha1, attributes, thumbnail, preview FROM permanodes WHERE search_high LIKE ? OR search_low LIKE ?" ("%"<>t<>"%","%"<>t<>"%")

  getThumbnail (SL conn) (SHA1 sha) =
    do res <- listToMaybe <$> query conn "SELECT thumbnail FROM permanodes WHERE sha1 = ?" (Only sha)
       case res of
         Nothing         -> return Nothing
         Just (Only jpg) -> return (Just jpg)
