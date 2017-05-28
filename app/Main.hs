{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Logging                    (log', withStderrLogging)
import           Data.Aeson
import           Data.Binary.Builder                (Builder)
import qualified Data.Binary.Builder                as Builder
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy               as BL
import           Data.Map                           (Map)
import qualified Data.Map                           as M
import           Data.Maybe                         (catMaybes, fromMaybe)
import           Data.Monoid                        ((<>))
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Database.PostgreSQL.Simple         (Connection, Only (..),
                                                     connectPostgreSQL, query)
import           Network.HTTP.Types                 (hContentType)
import           Network.HTTP.Types.Status          (status200)
import           Network.Wai                        (Response, responseBuilder,
                                                     responseLBS)
import           Network.Wai.Handler.Warp           (runEnv)
import           System.Environment                 (getEnv, lookupEnv)
import           System.FilePath                    (takeExtension)
import           Web.Fn
import qualified Web.Larceny                        as L

import           Database.Shed.BlobServer
import           Database.Shed.BlobServer.Directory
import           Database.Shed.Files
import           Database.Shed.Images
import           Database.Shed.Indexer
import           Database.Shed.Signing
import           Database.Shed.Types

type Fill = L.Fill ()
type Library = L.Library ()
type Substitutions = L.Substitutions ()

data Ctxt = Ctxt { _req     :: FnRequest
                 , _store   :: FileStore
                 , _db      :: Connection
                 , _library :: Library
                 , _key     :: Key
                 }
instance RequestContext Ctxt where
  getRequest = _req
  setRequest c r = c { _req = r }

render :: Ctxt -> T.Text -> IO (Maybe Response)
render ctxt = renderWith ctxt mempty

renderWith :: Ctxt -> Substitutions -> T.Text -> IO (Maybe Response)
renderWith ctxt subs tpl =
  do t <- L.renderWith (_library ctxt) subs () (T.splitOn "/" tpl)
     case t of
       Nothing -> return Nothing
       Just t' -> okHtml t'


initializer :: IO Ctxt
initializer = do
  lib <- L.loadTemplates "templates" L.defaultOverrides
  db <- T.pack . fromMaybe "shed" <$> lookupEnv "INDEX"
  conn <- connectPostgreSQL $ T.encodeUtf8 $ "dbname='" <> db <> "'"
  pth <- T.pack . fromMaybe "." <$> (lookupEnv "BLOBS")
  let store = FileStore pth
  keyid <- T.pack <$> getEnv "KEY"
  keyblob <- getPubKey keyid
  ref <- writeBlob store keyblob
  let key = Key keyid ref
  log' $ "Opening up the shed (" <> pth <> " & " <> db <> ")..."
  return (Ctxt defaultFnRequest store conn lib key)

main :: IO ()
main = withStderrLogging $
  do ctxt <- initializer
     runEnv 3000 $ toWAI ctxt site

instance FromParam SHA1 where
  fromParam [x] | "sha1-" `T.isPrefixOf` x = Right $ SHA1 x
  fromParam []  = Left ParamMissing
  fromParam _   = Left ParamTooMany

site :: Ctxt -> IO Response
site ctxt = route ctxt [ end // param "page"          ==> indexH
                       , path "static" ==> staticServe "static"
                       , segment // path "thumb" ==> thumbH
                       , segment ==> smartBlobH
                       , path "raw" // segment ==> blobH
                       , path "upload" // file "file" !=> uploadH
                       ]
                  `fallthrough` notFoundText "Page not found."

readFileBytes :: Ctxt -> [Part] -> IO Builder
readFileBytes ctxt ps =
  do bs <- catMaybes <$> mapM (\(Part sha _) ->
                                  readBlob (_store ctxt) sha) ps
     return $ foldl (\builder st ->
               Builder.append builder
               (Builder.fromLazyByteString st))
       Builder.empty
       bs

indexH :: Ctxt -> Maybe Int -> IO (Maybe Response)
indexH ctxt page = do
  rs <- query (_db ctxt) "SELECT attributes->>'camliContent' FROM permanodes WHERE show_in_ui = true ORDER BY sha1 DESC LIMIT 20 OFFSET ?" (Only (20 * (fromMaybe 0 page)))
  fs <- catMaybes <$> mapM (\(Only sha) ->
                              (fmap (sha, )) <$> ((>>= decode) <$> readBlob (_store ctxt) sha))
                           rs
  renderWith ctxt
             (L.subs [("next-page", L.textFill $ maybe "1" (T.pack . show . (+1)) page)
                     ,("files", L.mapSubs (\f -> case f of
                                                   (SHA1 sha, FileBlob name _) -> L.subs [("is-file", L.fillChildren)
                                                                                         ,("not-file", L.textFill "")
                                                                                         ,("sha", L.textFill sha)
                                                                                         ,("name", L.textFill name)]
                                                   _ -> L.subs [("is-file", L.textFill "")
                                                               ,("not-file", L.fillChildren)])
                                fs)])
             "index"

blobH :: Ctxt -> SHA1 -> IO (Maybe Response)
blobH ctxt sha@(SHA1 s) =
  do log' $ "Reading " <> s
     res' <- readBlob (_store ctxt) sha
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
thumbH ctxt sha@(SHA1 s) =
  do log' $ "Thumbnail of " <> s
     res' <- readBlob' (_store ctxt) sha
     case res' of
       Nothing  -> return Nothing
       Just (Bytes bs) -> renderIcon
       Just (FileBlob name ps) -> do
         let content = maybe [] (\c -> [(hContentType, c)]) $ M.lookup (takeExtension $ T.unpack name) mimeMap
         builder <- readFileBytes ctxt ps
         let dat = BL.toStrict $ Builder.toLazyByteString builder
         res <- getExifThumbnail dat
         case res of
           Nothing -> renderIcon
           Just jpg ->
             return $ Just $ responseBuilder status200 [(hContentType, "image/jpeg")] (Builder.fromByteString jpg)


smartBlobH :: Ctxt -> SHA1 -> IO (Maybe Response)
smartBlobH ctxt sha@(SHA1 s) =
  do log' $ "Reading (smart) " <> s
     res' <- readBlob' (_store ctxt) sha
     case res' of
       Nothing  -> return Nothing
       Just (Bytes bs) -> return $ Just $ responseLBS status200 [] bs
       Just (FileBlob name ps) -> do
         let content = maybe [] (\c -> [(hContentType, c)]) $ M.lookup (takeExtension $ T.unpack name) mimeMap
         builder <- readFileBytes ctxt ps
         return $ Just $ responseBuilder status200 content builder

uploadH :: Ctxt -> File -> IO (Maybe Response)
uploadH ctxt f = do log' $ "Uploading " <> fileName f <> "..."
                    addFile (_db ctxt) (_key ctxt) (_store ctxt) f
                    okText "OK"
