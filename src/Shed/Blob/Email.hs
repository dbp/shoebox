{-# LANGUAGE OverloadedStrings #-}
-- This module contains logic pertaining to camliType "email"
module Shed.Blob.Email where

import           Control.Arrow            ((***))
import           Control.Logging          (log')
import           Control.Monad            (when)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson.Types
import           Data.Binary.Builder      (Builder)
import qualified Data.Binary.Builder      as Builder
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Char                (toLower)
import           Data.Maybe               (fromMaybe, listToMaybe)
import           Data.MBox                (body, fromLine, headers, parseMBox)
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TL
import           Network.Wai              (Response)
import           System.FilePath          (takeExtension)
import           Web.Fn                   (File (..))
import qualified Web.Larceny              as L

import qualified Shed.Blob.File           as File
import qualified Shed.Blob.Permanode      as Permanode
import           Shed.BlobServer
import           Shed.IndexServer
import           Shed.Types
import           Shed.Util

data Header = Header Text Text deriving Show

instance FromJSON Header where
  parseJSON (Object v) = Header <$> v .: "name"
                                <*> v .: "value"
  parseJSON invalid = typeMismatch "Header" invalid

instance ToJSON Header where
  toJSON (Header name value) = object ["name" .= name
                                      ,"value" .= value]

getHeader :: [Header] -> Text -> Maybe Text
getHeader hs k = (\(Header _ v) -> v) <$> listToMaybe (filter (\(Header k' _) -> k == k') hs)

data EmailBlob = EmailBlob Text [Header] [File.Part]

instance FromJSON EmailBlob where
  parseJSON (Object v) = (do t <- v .: "camliType"
                             if t == ("email" :: Text) then
                               EmailBlob <$> v .: "from"
                                         <*> v .: "headers"
                                         <*> v .: "body"
                             else fail "Not an email")
  parseJSON invalid    = typeMismatch "Blob" invalid


instance ToJSON EmailBlob where
  toJSON (EmailBlob from headers body) =
    object ["camliVersion" .= (1 :: Int)
           ,"camliType" .= ("email" :: Text)
           ,"from" .= from
           ,"headers" .= headers
           ,"body" .= body]

indexBlob :: SomeBlobServer -> SomeIndexServer -> SHA1 -> EmailBlob -> IO ()
indexBlob store serv sha (EmailBlob from headers body) = do
  exists <- permanodeHasContent serv sha
  when exists $ do
    setPermanodeShowInUI serv sha
    setSearchHigh serv sha $
      T.concat [fromMaybe "" $ getHeader headers "From", "\n"
               ,fromMaybe "" $ getHeader headers "Subject", "\n"]
    b <- File.readFileBytes store body
    let body' = T.decodeUtf8 $ BL.toStrict $ Builder.toLazyByteString b
    setSearchLow serv sha body'
    let preview = (maybe "" (<> "\n") (getHeader headers "Subject"))
               <> (maybe "" (\x -> "From: " <> x <> "\n") (getHeader headers "From"))
               <> (maybe "" (\x -> "Date: " <> x <> "\n") (getHeader headers "Date"))
    setPermanodePreview serv sha preview

recognizeBlob :: SomeBlobServer -> SomeIndexServer -> Key -> File -> (File -> IO ()) -> IO ()
recognizeBlob store serv key file recognize =
    case map toLower $ takeExtension (T.unpack $ fileName file) of
      ".mbox" -> emailextract store serv key file
      ext     -> return ()

emailextract :: SomeBlobServer -> SomeIndexServer -> Key -> File -> IO ()
emailextract store serv key f = do
  let messages = parseMBox (TL.decodeUtf8 (fileContent f))
  mapM_ (\m -> do log' $ "Adding email '" <> TL.toStrict (fromLine m) <> "'."
                  brefs <- File.addChunks store (BL.toStrict $ TL.encodeUtf8 $ body m)
                  let email = EmailBlob (TL.toStrict $ fromLine m)
                                       (map (uncurry Header . (TL.toStrict *** TL.toStrict)) (headers m))
                                       (map (uncurry File.Part) brefs)
                  let emailblob = BL.toStrict $ encodePretty email
                  exists <- statBlob store emailblob
                  if exists then return () else do
                    (SHA1 eref) <- writeBlob store emailblob
                    (pref, permablob) <- Permanode.addPermanode store key
                    (cref, claimblob) <- Permanode.setAttribute store key pref "camliContent" eref
                    Permanode.indexBlob store serv pref permablob
                    Permanode.indexBlob store serv cref claimblob
                    indexBlob store serv (SHA1 eref) email
        ) messages


toHtml :: SomeBlobServer -> SomeIndexServer -> (L.Substitutions () -> Text -> IO (Maybe Response)) -> SHA1 -> BL.ByteString -> IO (Maybe Response)
toHtml store serv renderWith sha bs = do
  case decode bs of
    Just (EmailBlob from headers body) -> do
      b <- File.readFileBytes store body
      let body = T.decodeUtf8 $ BL.toStrict $ Builder.toLazyByteString b
      renderWith (L.subs
                   [("from", L.textFill $ getHeader headers "From")
                   ,("subject", L.textFill $ getHeader headers "Subject")
                   ,("message-id", L.textFill $ getHeader headers "Message-ID")
                   ,("date", L.textFill $ getHeader headers "Date")
                   ,("body-content", L.textFill body)])
        "email"
    _ -> return Nothing
  where getHeader hs h = let (Header _ v) = fromMaybe (Header "" "") $ listToMaybe $ filter (\(Header n v) -> n == h) hs in v
