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

import qualified Shoebox.Blob.Email      as Email
import qualified Shoebox.Blob.File       as File
import           Shoebox.BlobServer
import           Shoebox.Images
import           Shoebox.IndexServer
import           Shoebox.Signing
import           Shoebox.Types

decoders :: SomeBlobServer
         -> SomeIndexServer
         -> SHA224
         -> BL.ByteString
         -> [Maybe (IO ())]
decoders st se sha d =
  [File.indexBlob st se sha <$> decode d
  ,Email.indexBlob st se sha <$> decode d
  ]

index :: SomeBlobServer -> SomeIndexServer -> IO ()
index a s = do
  enumerateBlobs a $ \sha dat -> do
    putStrLn $ show sha
    case msum $ decoders a s sha dat of
      Just a  -> a
      Nothing -> return ()
  putStrLn "\rDONE                                                            "