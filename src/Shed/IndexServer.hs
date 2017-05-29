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

data AnIndexServer = forall s. IndexServer s => AnIndexServer s
