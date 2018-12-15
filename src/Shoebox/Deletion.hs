{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shoebox.Deletion where

import Data.Text (Text)
import           Data.Aeson
import           Data.Aeson.Types
import Data.Time.Clock

import Shoebox.Blob.File
import           Shoebox.Types
import Shoebox.BlobServer
import Shoebox.Blob.Delete
import Shoebox.Blob.Replace


delete :: SomeBlobServer -> IO ()
delete store = enumerateBlobs store $
  \sha content ->
    do case decode content of
         Nothing -> return ()
         Just (DeleteBlob target _) ->
           deleteBlob store target
       case decode content of
         Nothing -> return ()
         Just (ReplaceBlob old _ _) ->
           deleteBlob store old

-- NOTE(dbp 2018-12-15): When you want to delete a single blob,
-- sometimes others are logically connected. i.e., a file blob
-- should also take down the content blob(s).
findConnectedBlobs :: SomeBlobServer -> SHA224 -> IO [SHA224]
findConnectedBlobs store sha = do
  bs <- readBlob store sha
  let iffile = case decode =<< bs of
                 Just (FileBlob _ ps) -> map (\(Part r _) -> r) ps
                 Nothing -> []
  return (sha:iffile)
