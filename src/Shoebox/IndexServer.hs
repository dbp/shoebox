{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
module Shoebox.IndexServer where

import           Data.ByteString (ByteString)
import           Data.Text       (Text)
import           Shoebox.Types

class IndexServer a where
  wipe :: a -> IO ()

  makeItem :: a -> SHA224 -> IO ()
  removeItem :: a -> SHA224 -> IO ()
  setSearchHigh :: a -> SHA224 -> Text -> IO ()
  getSearchHigh :: a -> SHA224 -> IO (Maybe Text)
  setSearchLow :: a -> SHA224 -> Text -> IO ()
  setThumbnail :: a -> SHA224 -> ByteString -> IO ()
  setMedium :: a -> SHA224 -> ByteString -> IO ()
  setPreview :: a -> SHA224 -> Text -> IO ()

  showInRoot :: a -> SHA224 -> IO ()

  getItem :: a -> SHA224 -> IO (Maybe Item)
  getItems :: a -> Int -> IO [Item]
  search :: a -> Text -> IO [Item]
  getThumbnail :: a -> SHA224 -> IO (Maybe ByteString)
  getMedium :: a -> SHA224 -> IO (Maybe ByteString)

  getRedirections :: a -> SHA224 -> IO [SHA224]
  setRedirection :: a -> SHA224 -> SHA224 -> IO ()

  getUrls :: a -> Text -> IO [SHA224]
  setUrl :: a -> Text -> SHA224 -> SHA224 -> IO ()
  getWithUrl :: a -> SHA224 -> IO [(SHA224, Text)]
  removeUrl :: a -> SHA224 -> IO ()

  setNote :: a -> Text -> SHA224 -> SHA224 -> IO ()
  removeNote :: a -> SHA224 -> IO ()
  getNotes :: a -> SHA224 -> IO [(SHA224, Text)]

data SomeIndexServer = forall s. IndexServer s => SomeIndexServer s

instance IndexServer SomeIndexServer where
  wipe (SomeIndexServer s) = wipe s
  makeItem (SomeIndexServer s) = makeItem s
  removeItem (SomeIndexServer s) = removeItem s
  setSearchHigh (SomeIndexServer s) = setSearchHigh s
  getSearchHigh (SomeIndexServer s) = getSearchHigh s
  setSearchLow (SomeIndexServer s) = setSearchLow s
  setThumbnail (SomeIndexServer s) = setThumbnail s
  setMedium (SomeIndexServer s) = setMedium s
  setPreview (SomeIndexServer s) = setPreview s
  showInRoot (SomeIndexServer s) = showInRoot s
  getItem (SomeIndexServer s) = getItem s
  getItems (SomeIndexServer s) = getItems s
  search (SomeIndexServer s) = search s
  getThumbnail (SomeIndexServer s) = getThumbnail s
  getMedium (SomeIndexServer s) = getMedium s
  getRedirections (SomeIndexServer s) = getRedirections s
  setRedirection (SomeIndexServer s) = setRedirection s
  getUrls (SomeIndexServer s) = getUrls s
  setUrl (SomeIndexServer s) = setUrl s
  getWithUrl (SomeIndexServer s) = getWithUrl s
  removeUrl (SomeIndexServer s) = removeUrl s
  setNote (SomeIndexServer s) = setNote s
  removeNote (SomeIndexServer s) = removeNote s
  getNotes (SomeIndexServer s) = getNotes s
