{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Shoebox.Indexer where

import           Control.Applicative  ((<|>))
import           Control.Logging      (log')
import           Control.Monad        (msum, void, when)
import           Data.Aeson           (decode)
import qualified Data.ByteString.Lazy as BL
import           Data.Monoid          ((<>))
import qualified Data.Text            as T

import qualified Shoebox.Blob.Box     as Box
import qualified Shoebox.Blob.Email   as Email
import qualified Shoebox.Blob.File    as File
import qualified Shoebox.Blob.Replace as Replace
import           Shoebox.BlobServer
import           Shoebox.Images
import           Shoebox.IndexServer
import           Shoebox.Types

decoders :: SomeBlobServer
         -> SomeIndexServer
         -> SHA224
         -> BL.ByteString
         -> [Maybe (IO ())]
decoders st se sha d =
  [File.indexBlob st se sha <$> decode d
  ,Box.indexBlob st se sha <$> decode d
  ,Email.indexBlob st se sha <$> decode d
  ,Replace.indexBlob st se sha <$> decode d
  ]

index :: SomeBlobServer -> SomeIndexServer -> IO ()
index a s = do
  enumerateBlobs a $ \sha dat -> do
    log' $ "INDEX" <> T.pack (show sha)
    case msum $ decoders a s sha dat of
      Just a  -> a
      Nothing -> return ()

