{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Logging             (log', withStderrLogging)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty    (encodePretty)
import           Data.Binary.Builder         (Builder)
import qualified Data.Binary.Builder         as Builder
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Lazy        as BL
import           Data.ByteString.Unsafe      (unsafeUseAsCStringLen)
import qualified Data.HashTable.IO           as H
import           Data.Map                    (Map)
import qualified Data.Map                    as M
import           Data.Maybe                  (catMaybes, fromMaybe, isJust,
                                              listToMaybe)
import           Data.Monoid                 ((<>))
import           Data.String                 (fromString)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Database.PostgreSQL.Simple  as PG
import qualified Database.SQLite.Simple      as SQLITE
import qualified HTMLEntities.Text           as HE
import           Magic                       (MagicFlag (MagicMimeType),
                                              magicCString, magicLoadDefault,
                                              magicOpen)
import           Network.HTTP.Types          (hContentType)
import           Network.HTTP.Types.Status   (status200)
import           Network.Wai                 (Response, rawPathInfo,
                                              requestMethod, responseBuilder,
                                              responseLBS)
import           Network.Wai.Handler.Warp    (runEnv)
import           System.Environment          (getEnv, lookupEnv)
import           System.FilePath             (takeExtension)
import           Text.RE.Replace
import           Text.RE.TDFA.Text
import           Web.Fn
import qualified Web.Larceny                 as L

import           Shed.BlobServer
import           Shed.BlobServer.Directory
import           Shed.BlobServer.Memory
import           Shed.Files
import           Shed.Images
import           Shed.Importers
import           Shed.Indexer
import           Shed.IndexServer
import           Shed.IndexServer.Postgresql
import           Shed.IndexServer.Sqlite
import           Shed.Signing
import           Shed.Types

type Fill = L.Fill ()
type Library = L.Library ()
type Substitutions = L.Substitutions ()

data Ctxt = Ctxt { _req     :: FnRequest
                 , _store   :: ABlobServer
                 , _db      :: AnIndexServer
                 , _library :: Library
                 , _key     :: Key
                 }

instance RequestContext Ctxt where
  getRequest = _req
  setRequest c r = c { _req = r }

render :: Ctxt -> Text -> IO (Maybe Response)
render ctxt = renderWith ctxt mempty

renderWith :: Ctxt -> Substitutions -> Text -> IO (Maybe Response)
renderWith ctxt subs tpl =
  do t <- L.renderWith (_library ctxt) subs () (T.splitOn "/" tpl)
     case t of
       Nothing -> return Nothing
       Just t' -> okHtml t'


initializer :: IO Ctxt
initializer = do
  lib <- L.loadTemplates "templates" L.defaultOverrides
  pth' <- fmap T.pack <$> (lookupEnv "BLOBS")
  (store, pth) <- case pth' of
                    Just pth'' -> return (ABlobServer (FileStore pth''), pth'')
                    Nothing    -> do
                      ht <- H.new
                      return (ABlobServer (MemoryStore ht), ":memory:")
  db' <- fmap T.pack <$> lookupEnv "INDEX"
  (serv, nm) <- case db' of
                  Just db -> do c <- PG.connectPostgreSQL $ T.encodeUtf8 $ "dbname='" <> db <> "'"
                                return (AnIndexServer (PG c), db)
                  Nothing -> do sql <- readFile "migrations/sqlite.sql"
                                c <- SQLITE.open ":memory:"
                                SQLITE.execute_ c (fromString sql)
                                let serv = AnIndexServer (SL c)
                                log' "Running indexer to populate :memory: index."
                                -- NOTE(dbp 2017-05-29): Run many times because
                                -- we need permanodes in DB before files stored
                                -- in them are indexed
                                index store serv
                                index store serv
                                index store serv
                                return (serv, ":memory:")
  keyid <- T.pack <$> getEnv "KEY"
  keyblob <- getPubKey keyid
  ref <- writeBlob store keyblob
  let key = Key keyid ref
  log' $ "Opening the Shed [Blobs " <> pth <> " Index " <> nm <> "]"
  return (Ctxt defaultFnRequest store serv lib key)

main :: IO ()
main = withStderrLogging $
  do ctxt <- initializer
     runEnv 3000 $ toWAI ctxt site

instance FromParam SHA1 where
  fromParam [x] | "sha1-" `T.isPrefixOf` x = Right $ SHA1 x
  fromParam []  = Left ParamMissing
  fromParam _   = Left ParamTooMany

site :: Ctxt -> IO Response
site ctxt = do
  log' $ T.decodeUtf8 (requestMethod (fst $ _req ctxt)) <> " " <> T.decodeUtf8 (rawPathInfo (fst $ _req ctxt))
  route ctxt [ end // param "page"          ==> indexH
                       , path "static" ==> staticServe "static"
                       , segment // path "thumb" ==> thumbH
                       , segment ==> smartBlobH
                       , path "file" // segment ==> fileH
                       , path "raw" // segment ==> rawH
                       , path "blob" // segment ==> blobH
                       , path "upload" // file "file" !=> uploadH
                       , path "search" // param "q" ==> searchH
                       ]
    `fallthrough` do r <- render ctxt "404"
                     case r of
                       Just r' -> return r'
                       Nothing -> notFoundText "Page not found"

permanodeSubs :: Permanode -> Substitutions
permanodeSubs (Permanode (SHA1 sha) attrs thumb prev) =
  L.subs [("permanodeRef", L.textFill sha)
         ,("contentRef", L.textFill $ attrs M.! "camliContent")
         ,("has-thumbnail", justFill thumb)
         ,("no-thumbnail", nothingFill thumb)
         ,("has-preview", justFill prev)
         ,("preview", L.rawTextFill $ maybe "" (T.replace "\n" "</p><p>" . HE.text) prev)]
  where
    justFill m = if isJust m then L.fillChildren else L.textFill ""
    nothingFill m = if isJust m then L.textFill "" else L.fillChildren

indexH :: Ctxt -> Maybe Int -> IO (Maybe Response)
indexH ctxt page = do
  ps <- getPermanodes (_db ctxt) (fromMaybe 0 page)
  renderWith ctxt
    (L.subs [("has-more", L.fillChildren)
            ,("next-page", L.textFill $ maybe "1" (T.pack . show . (+1)) page)
            ,("permanodes", L.mapSubs permanodeSubs ps)
            ,("q", L.textFill "")])
    "index"

searchH :: Ctxt -> Text -> IO (Maybe Response)
searchH ctxt q = do
  if T.strip q == "" then redirect "/" else do
    ps <- search (_db ctxt) q
    if length ps == 0 then redirect "/" else
      renderWith ctxt
        (L.subs [("has-more", L.textFill "")
                ,("q", L.textFill q)
                ,("permanodes", L.mapSubs permanodeSubs ps)])
        "index"

hyperLinkEscape :: Text -> Text
hyperLinkEscape t =
  replaceAll "<a href=\"/blob/${sha}\">${sha}</a>" $
    (HE.text t) *=~ [re|${sha}(sha1-[0-9a-f]{40})|]

blobH :: Ctxt -> SHA1 -> IO (Maybe Response)
blobH ctxt sha = do
  res' <- readBlob' (_store ctxt) sha
  case res' of
    Nothing  -> rawH ctxt sha
    Just (Bytes bs) -> do
      m <- magicOpen [MagicMimeType]
      magicLoadDefault m
      let b = BL.toStrict bs
      mime <- unsafeUseAsCStringLen b (magicCString m)
      let display = renderWith ctxt (L.subs [("content", L.textFill (T.decodeUtf8 b))]) "blob"
      case mime of
        "text/plain" -> display
        "text/html"  -> display
        _            -> rawH ctxt sha
    Just b -> do
      extra <-
        case b of
          PermanodeBlob _ _ -> do
            mp <- getPermanode (_db ctxt) sha
            return $ case mp of
                       Nothing -> Nothing
                       Just p  -> Just $ BL.toStrict $ encodePretty (toJSON $ attributes p)
          _ -> return Nothing
      Just blob <- fmap (T.decodeUtf8 . BL.toStrict) <$> readBlob (_store ctxt) sha
      renderWith ctxt (L.subs [("content", L.rawTextFill (hyperLinkEscape blob))
                              ,("extra", if isJust extra then
                                   L.fillChildrenWith (L.subs [("content", L.rawTextFill (hyperLinkEscape $ maybe "" T.decodeUtf8 extra))]) else L.textFill "")])
           "blob"


rawH :: Ctxt -> SHA1 -> IO (Maybe Response)
rawH ctxt sha@(SHA1 s) =
  do res' <- readBlob (_store ctxt) sha
     case res' of
       Nothing  -> return Nothing
       Just res -> return $ Just $ responseLBS status200 [] res


-- NOTE(dbp 2015-11-05): This list taken from snap-core, BSD3 licensed.
mimeMap :: Map String ByteString
mimeMap =  M.fromList [
  ( ".asc"     , "text/plain"                        ),
  ( ".asf"     , "video/x-ms-asf"                    ),
  ( ".asx"     , "video/x-ms-asf"                    ),
  ( ".avi"     , "video/x-msvideo"                   ),
  ( ".bz2"     , "application/x-bzip"                ),
  ( ".c"       , "text/plain"                        ),
  ( ".class"   , "application/octet-stream"          ),
  ( ".conf"    , "text/plain"                        ),
  ( ".cpp"     , "text/plain"                        ),
  ( ".css"     , "text/css"                          ),
  ( ".cxx"     , "text/plain"                        ),
  ( ".dtd"     , "text/xml"                          ),
  ( ".dvi"     , "application/x-dvi"                 ),
  ( ".gif"     , "image/gif"                         ),
  ( ".gz"      , "application/x-gzip"                ),
  ( ".hs"      , "text/plain"                        ),
  ( ".htm"     , "text/html"                         ),
  ( ".html"    , "text/html"                         ),
  ( ".ico"     , "image/x-icon"                      ),
  ( ".jar"     , "application/x-java-archive"        ),
  ( ".jpeg"    , "image/jpeg"                        ),
  ( ".jpg"     , "image/jpeg"                        ),
  ( ".js"      , "text/javascript"                   ),
  ( ".json"    , "application/json"                  ),
  ( ".log"     , "text/plain"                        ),
  ( ".m3u"     , "audio/x-mpegurl"                   ),
  ( ".mov"     , "video/quicktime"                   ),
  ( ".mp3"     , "audio/mpeg"                        ),
  ( ".mpeg"    , "video/mpeg"                        ),
  ( ".mpg"     , "video/mpeg"                        ),
  ( ".ogg"     , "application/ogg"                   ),
  ( ".pac"     , "application/x-ns-proxy-autoconfig" ),
  ( ".pdf"     , "application/pdf"                   ),
  ( ".png"     , "image/png"                         ),
  ( ".ps"      , "application/postscript"            ),
  ( ".qt"      , "video/quicktime"                   ),
  ( ".sig"     , "application/pgp-signature"         ),
  ( ".spl"     , "application/futuresplash"          ),
  ( ".svg"     , "image/svg+xml"                     ),
  ( ".swf"     , "application/x-shockwave-flash"     ),
  ( ".tar"     , "application/x-tar"                 ),
  ( ".tar.bz2" , "application/x-bzip-compressed-tar" ),
  ( ".tar.gz"  , "application/x-tgz"                 ),
  ( ".tbz"     , "application/x-bzip-compressed-tar" ),
  ( ".text"    , "text/plain"                        ),
  ( ".tgz"     , "application/x-tgz"                 ),
  ( ".torrent" , "application/x-bittorrent"          ),
  ( ".ttf"     , "application/x-font-truetype"       ),
  ( ".txt"     , "text/plain"                        ),
  ( ".wav"     , "audio/x-wav"                       ),
  ( ".wax"     , "audio/x-ms-wax"                    ),
  ( ".wma"     , "audio/x-ms-wma"                    ),
  ( ".wmv"     , "video/x-ms-wmv"                    ),
  ( ".xbm"     , "image/x-xbitmap"                   ),
  ( ".xml"     , "text/xml"                          ),
  ( ".xpm"     , "image/x-xpixmap"                   ),
  ( ".xwd"     , "image/x-xwindowdump"               ),
  ( ".zip"     , "application/zip"                   ) ]

renderIcon :: IO (Maybe Response)
renderIcon = sendFile "static/icon.png"

thumbH :: Ctxt -> SHA1 -> IO (Maybe Response)
thumbH ctxt sha =
  do res <- getThumbnail (_db ctxt) sha
     case res of
       Nothing -> renderIcon
       Just jpg -> return $ Just $ responseBuilder status200 [(hContentType, "image/jpeg")] (Builder.fromByteString jpg)


smartBlobH :: Ctxt -> SHA1 -> IO (Maybe Response)
smartBlobH ctxt sha@(SHA1 s) =
  do res' <- readBlob' (_store ctxt) sha
     case res' of
       Nothing  -> return Nothing
       Just (Bytes bs) -> return $ Just $ responseLBS status200 [] bs
       Just (FileBlob name ps) -> do
         renderWith ctxt (L.subs [("name", L.textFill name)
                                 ,("fileRef", L.textFill s)])
           "file"
       Just (EmailBlob from headers body) -> do
         b <- readFileBytes (_store ctxt) body
         let body = T.decodeUtf8 $ BL.toStrict $ Builder.toLazyByteString b
         renderWith ctxt (L.subs
                     [("from", L.textFill $ getHeader headers "From")
                      ,("subject", L.textFill $ getHeader headers "Subject")
                      ,("message-id", L.textFill $ getHeader headers "Message-ID")
                      ,("date", L.textFill $ getHeader headers "Date")
                      ,("body-content", L.textFill body)])
           "email"
  where getHeader hs h = let (Header _ v) = fromMaybe (Header "" "") $ listToMaybe $ filter (\(Header n v) -> n == h) hs in v

fileH :: Ctxt -> SHA1 -> IO (Maybe Response)
fileH ctxt sha =
  do res' <- readBlob' (_store ctxt) sha
     case res' of
       Just (FileBlob name ps) -> do
         let content = maybe [] (\c -> [(hContentType, c)]) $ M.lookup (takeExtension $ T.unpack name) mimeMap
         builder <- readFileBytes (_store ctxt) ps
         return $ Just $ responseBuilder status200 content builder
       _ -> return Nothing

uploadH :: Ctxt -> File -> IO (Maybe Response)
uploadH ctxt f = do log' $ "Uploading " <> fileName f <> "..."
                    process (_store ctxt) (_db ctxt) (_key ctxt)  f
                    okText "OK"
