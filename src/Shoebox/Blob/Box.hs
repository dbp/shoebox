{-# LANGUAGE OverloadedStrings #-}
module Shoebox.Blob.Box where

import           Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types
import Data.Time.Clock
import Data.Text (Text)
import Web.Fn
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy     as BL
import           Network.Wai              (Response)
import qualified Web.Larceny as L
import Data.Maybe (catMaybes)
import Control.Monad (void)
import Crypto.Random (getRandomBytes)
import qualified Crypto.Hash as Hash
import qualified Data.Text.Encoding      as T
import qualified Data.ByteString.Builder as Builder
import Data.ByteArray (convert)

import Shoebox.Items
import Shoebox.BlobServer
import qualified Shoebox.Blob.Replace as Replace
import Shoebox.IndexServer
import Shoebox.Types

data BoxBlob = BoxBlob { random :: Text
                       , title :: Text
                       , contents :: [SHA224]
                       , preview :: Maybe SHA224
                       }

instance FromJSON BoxBlob where
  parseJSON (Object v) = (do t <- v .: "type"
                             if t == ("box" :: Text) then
                               BoxBlob <$> v .: "random"
                                       <*> v .: "title"
                                       <*> v .: "contents"
                                       <*> v .: "preview"
                               else fail "Not a delete")
  parseJSON invalid    = typeMismatch "BoxBlob" invalid

instance ToJSON BoxBlob where
  toJSON (BoxBlob r t c p) = object ["version" .= (1 :: Int)
                                    ,"type" .= ("box" :: Text)
                                    ,"random" .= r
                                    ,"title" .= t
                                    ,"contents" .= c
                                    ,"preview" .= p]

indexBlob :: SomeBlobServer -> SomeIndexServer -> SHA224 -> BoxBlob -> IO ()
indexBlob store serv sha (BoxBlob _ title contents prev) = do
  makeItem serv sha
  setSearchHigh serv sha title
  setPreview serv sha title
  case prev of
    Nothing -> return ()
    Just s -> do mthum <- getThumbnail serv s
                 case mthum of
                   Nothing -> return ()
                   Just thum -> setThumbnail serv sha thum


toHtml :: SomeBlobServer -> SomeIndexServer -> (L.Substitutions () -> Text -> IO (Maybe Response)) -> SHA224 -> BL.ByteString -> IO (Maybe Response)
toHtml store serv renderWith (SHA224 sha) bs =
  case decode bs of
    Just (BoxBlob _ title contents _) -> do
      is <- fmap catMaybes $ mapM (getItem serv) contents
      renderWith (L.subs [("box-ref", L.textFill sha)
                         ,("box-title", L.textFill title)
                         ,("items", L.mapSubs itemSubs is)])
        "box"
    _ -> return Nothing

updateBox :: SomeBlobServer -> SomeIndexServer -> SHA224 -> BoxBlob -> IO ()
updateBox store serv oldRef (BoxBlob _ t c p) = do 
  now <- getCurrentTime
  bytes <- getRandomBytes 32 :: IO ByteString
  let digest = Hash.hash bytes :: Hash.Digest Hash.SHA1
  let rand = T.decodeUtf8 $ BL.toStrict $ Builder.toLazyByteString $ Builder.byteStringHex (convert digest)

  let withrand = BoxBlob rand t c p
  newRef <- writeBlob store (BL.toStrict $ encodePretty withrand)
  indexBlob store serv newRef withrand
  let repl = Replace.ReplaceBlob oldRef newRef now
  r <- writeBlob store (BL.toStrict $ encodePretty repl)
  Replace.indexBlob store serv r repl
  removeItem serv oldRef
  void $ deleteBlob store oldRef
