{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
module Shed.IndexServer where

import           Data.ByteString (ByteString)
import           Data.Text       (Text)
import           Shed.Types

class IndexServer a where
  wipe :: a -> IO ()

  makePermanode :: a -> SHA1 -> IO ()
  setPermanodeAttribute :: a -> SHA1 -> Text -> Text -> IO ()
  permanodeHasContent :: a -> SHA1 -> IO Bool
  setPermanodeShowInUI :: a -> SHA1 -> IO ()
  setSearchHigh :: a -> SHA1 -> Text -> IO ()
  setSearchLow :: a -> SHA1 -> Text -> IO ()
  setPermanodeThumbnail :: a -> SHA1 -> ByteString -> IO ()
  setPermanodePreview :: a -> SHA1 -> Text -> IO ()

  getPermanode :: a -> SHA1 -> IO (Maybe Permanode)
  getPermanodes :: a -> Int -> IO [Permanode]
  search :: a -> Text -> IO [Permanode]
  getThumbnail :: a -> SHA1 -> IO (Maybe ByteString)

data SomeIndexServer = forall s. IndexServer s => SomeIndexServer s

instance IndexServer SomeIndexServer where
  wipe (SomeIndexServer s) = wipe s
  makePermanode (SomeIndexServer s) = makePermanode s
  setPermanodeAttribute (SomeIndexServer s) = setPermanodeAttribute s
  permanodeHasContent (SomeIndexServer s) = permanodeHasContent s
  setPermanodeShowInUI (SomeIndexServer s) = setPermanodeShowInUI s
  setSearchHigh (SomeIndexServer s) = setSearchHigh s
  setSearchLow (SomeIndexServer s) = setSearchLow s
  setPermanodeThumbnail (SomeIndexServer s) = setPermanodeThumbnail s
  setPermanodePreview (SomeIndexServer s) = setPermanodePreview s
  getPermanode (SomeIndexServer s) = getPermanode s
  getPermanodes (SomeIndexServer s) = getPermanodes s
  search (SomeIndexServer s) = search s
  getThumbnail (SomeIndexServer s) = getThumbnail s
