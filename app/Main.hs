{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Logging                    (log', withStderrLogging)
import           Data.Aeson
import qualified Data.Binary.Builder                as Builder
import           Data.ByteString                    (ByteString)
import           Data.Map                           (Map)
import qualified Data.Map                           as M
import           Data.Maybe                         (catMaybes, fromMaybe)
import           Data.Monoid                        ((<>))
import qualified Data.Text                          as T
import           Database.PostgreSQL.Simple         (Connection, Only (..),
                                                     connectPostgreSQL, query_)
import           Network.HTTP.Types                 (hContentType)
import           Network.HTTP.Types.Status          (status200)
import           Network.Wai                        (Response, responseBuilder,
                                                     responseLBS)
import           Network.Wai.Handler.Warp           (runEnv)
import           System.Environment                 (lookupEnv)
import           System.FilePath                    (takeExtension)
import           Web.Fn

import           Database.Shed.BlobServer
import           Database.Shed.BlobServer.Directory
import           Database.Shed.Indexer
import           Database.Shed.Types

data Ctxt = Ctxt { _req   :: FnRequest
                 , _store :: FileStore
                 , _db    :: Connection
                 }
instance RequestContext Ctxt where
  getRequest = _req
  setRequest c r = c { _req = r }

initializer :: IO Ctxt
initializer = do
  conn <- connectPostgreSQL "dbname='shed'"
  pth <- T.pack . fromMaybe "." <$> (lookupEnv "BLOBS")
  return (Ctxt defaultFnRequest (FileStore pth) conn)

main :: IO ()
main = withStderrLogging $
  do ctxt <- initializer
     log' "Opening up the shed..."
     runEnv 3000 $ toWAI ctxt site

instance FromParam SHA1 where
  fromParam [x] | "sha1-" `T.isPrefixOf` x = Right $ SHA1 x
  fromParam []  = Left ParamMissing
  fromParam _   = Left ParamTooMany

site :: Ctxt -> IO Response
site ctxt = route ctxt [ end                   ==> indexH
                       , segment ==> smartBlobH
                       , path "raw" // segment ==> blobH
                       ]
                  `fallthrough` notFoundText "Page not found."

indexH :: Ctxt -> IO (Maybe Response)
indexH ctxt = do
  rs <- query_ (_db ctxt) "SELECT attributes->>'camliContent' as sha1 FROM permanodes WHERE show_in_ui = true"
  fs <- catMaybes <$> mapM (\(Only sha) ->
                              (fmap (sha, )) <$> ((>>= decode) <$> readBlob (_store ctxt) sha))
                           rs
  okHtml $ "<doctype !html><html><body><ul>" <> T.concat (map render fs) <> "</ul></body></html>"
  where render (SHA1 sha, FileBlob name _) =
          "<li><a href='/" <> sha <> "'>" <> name <> "</a></li>"
        render _ = "<li>Not a file</li>"

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


smartBlobH :: Ctxt -> SHA1 -> IO (Maybe Response)
smartBlobH ctxt sha@(SHA1 s) =
  do log' $ "Reading (smart) " <> s
     res' <- readBlob' (_store ctxt) sha
     case res' of
       Nothing  -> return Nothing
       Just (Bytes bs) -> return $ Just $ responseLBS status200 [] bs
       Just (FileBlob name ps) -> do
         let content = maybe [] (\c -> [(hContentType, c)]) $ M.lookup (takeExtension $ T.unpack name) mimeMap
         bs <- catMaybes <$> mapM (\(Part sha _) ->
                                     readBlob (_store ctxt) sha) ps
         let builder = foldl (\builder st ->
                                Builder.append builder
                                (Builder.fromLazyByteString st))
                       Builder.empty
                       bs
         return $ Just $ responseBuilder status200 content builder
