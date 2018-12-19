{-# LANGUAGE OverloadedStrings #-}
module Shoebox.Blob.Box where

import           Control.Monad            (void)
import qualified Crypto.Hash              as Hash
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson.Types
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Builder  as Builder
import qualified Data.ByteString.Lazy     as BL
import           Data.Maybe               (catMaybes, fromJust, isJust,
                                           isNothing)
import           Data.Text                (Text)
import qualified Data.Text.Encoding       as T
import           Data.Time.Clock
import           Network.Wai              (Response)
import           Web.Fn
import qualified Web.Larceny              as L

import qualified Shoebox.Blob.Replace     as Replace
import           Shoebox.BlobServer
import           Shoebox.IndexServer
import           Shoebox.Items
import           Shoebox.Types

data BoxBlob = BoxBlob { random   :: Text
                       , title    :: Text
                       , contents :: [SHA224]
                       , preview  :: Maybe SHA224
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
  showInRoot serv sha
  case prev of
    Nothing -> return ()
    Just s -> do mthum <- getThumbnail serv s
                 case mthum of
                   Nothing   -> return ()
                   Just thum -> setThumbnail serv sha thum


toHtml :: SomeBlobServer -> SomeIndexServer -> (L.Substitutions () -> Text -> IO (Maybe Response)) -> SHA224 -> BL.ByteString -> IO (Maybe Response)
toHtml store serv renderWith (SHA224 sha) bs =
  case decode bs of
    Just (BoxBlob _ title contents p) -> do
      is <- fmap catMaybes $ mapM (getItem serv) contents
      prev <- case p of
                Nothing -> return Nothing
                Just p' -> getItem serv p'
      urls <- getWithUrl serv (SHA224 sha)
      renderWith (L.subs [("box-ref", L.textFill sha)
                         ,("box-title", L.textFill title)
                         ,("urls", L.mapSubs urlSubs urls)
                         ,("items", L.mapSubs itemSubs is)
                         ,("has-preview", if isJust prev then L.fillChildren else L.textFill "")
                         ,("no-preview", if isNothing prev then L.fillChildren else L.textFill "")
                         ,("preview", if isJust prev then L.fillChildrenWith (prevSubs (fromJust prev)) else L.textFill "")])
        "box"
    _ -> return Nothing
  where urlSubs (SHA224 urlsha, url) =
          L.subs [("url-ref", L.textFill urlsha)
                 ,("url", L.textFill url)]
        prevSubs (Item (SHA224 ref) _ _ _) = L.subs [("ref", L.textFill ref)]

updateBox :: SomeBlobServer -> SomeIndexServer -> SHA224 -> BoxBlob -> IO ()
updateBox store serv oldRef (BoxBlob _ t c p) = do
  now <- getCurrentTime
  rand <- mkRandom
  let withrand = BoxBlob rand t c p
  newRef <- writeBlob store (BL.toStrict $ encodePretty withrand)
  indexBlob store serv newRef withrand
  let repl = Replace.ReplaceBlob oldRef newRef now
  r <- writeBlob store (BL.toStrict $ encodePretty repl)
  Replace.indexBlob store serv r repl
  removeItem serv oldRef
  void $ deleteBlob store oldRef
