{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Shed.Indexer where

import           Control.Applicative  ((<|>))
import           Control.Logging      (log')
import           Control.Monad        (msum, void, when)
import           Data.Aeson           (decode)
import qualified Data.ByteString.Lazy as BL
import           Data.Monoid          ((<>))

import qualified Shed.Blob.Email      as Email
import qualified Shed.Blob.File       as File
import qualified Shed.Blob.Permanode  as Permanode
import           Shed.BlobServer
import           Shed.Images
import           Shed.IndexServer
import           Shed.Signing
import           Shed.Types

decoders :: SomeBlobServer
         -> SomeIndexServer
         -> SHA1
         -> BL.ByteString
         -> [Maybe (IO ())]
decoders st se sha d =
  [Permanode.indexBlob st se sha <$> decode d
  ,File.indexBlob st se sha <$> decode d
  ,Email.indexBlob st se sha <$> decode d
  ]

index :: SomeBlobServer -> SomeIndexServer -> IO ()
index a s = do
  enumerateBlobs a $ \sha dat -> do
    putStr $ "\r" <> show sha
    case msum $ decoders a s sha dat of
      Just a  -> a
      Nothing -> return ()
  putStrLn "\rDONE                                            "
