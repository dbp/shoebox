{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
module Shed.IndexServer where

import           Data.ByteString (ByteString)
import           Data.Text       (Text)
import           Shed.Types

class IndexServer a where
  wipe :: a -> IO ()

  makeItem :: a -> SHA224 -> IO ()
  setSearchHigh :: a -> SHA224 -> Text -> IO ()
  setSearchLow :: a -> SHA224 -> Text -> IO ()
  setThumbnail :: a -> SHA224 -> ByteString -> IO ()
  setPreview :: a -> SHA224 -> Text -> IO ()

  getItem :: a -> SHA224 -> IO (Maybe Item)
  getItems :: a -> Int -> IO [Item]
  search :: a -> Text -> IO [Item]
  getThumbnail :: a -> SHA224 -> IO (Maybe ByteString)

data SomeIndexServer = forall s. IndexServer s => SomeIndexServer s

instance IndexServer SomeIndexServer where
  wipe (SomeIndexServer s) = wipe s
  makeItem (SomeIndexServer s) = makeItem s
  setSearchHigh (SomeIndexServer s) = setSearchHigh s
  setSearchLow (SomeIndexServer s) = setSearchLow s
  setThumbnail (SomeIndexServer s) = setThumbnail s
  setPreview (SomeIndexServer s) = setPreview s
  getItem (SomeIndexServer s) = getItem s
  getItems (SomeIndexServer s) = getItems s
  search (SomeIndexServer s) = search s
  getThumbnail (SomeIndexServer s) = getThumbnail s
