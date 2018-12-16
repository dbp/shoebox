{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Applicative            (liftA2)
import           Control.Logging                (log', withStderrLogging)
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Encode.Pretty       (encodePretty)
import           Data.Binary.Builder            (Builder)
import qualified Data.Binary.Builder            as Builder
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Lazy           as BL
import           Data.ByteString.Unsafe         (unsafeUseAsCStringLen)
import qualified Data.HashTable.IO              as H
import           Data.Map                       (Map)
import qualified Data.Map                       as M
import           Data.Maybe                     (catMaybes, fromJust, fromMaybe,
                                                 isJust, listToMaybe)
import           Data.Monoid                    ((<>))
import           Data.String                    (fromString)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Data.Time.Clock
import qualified Database.PostgreSQL.Simple     as PG
import qualified Database.SQLite.Simple         as SQLITE
import qualified HTMLEntities.Text              as HE
import           Magic                          (MagicFlag (MagicMimeType),
                                                 magicCString, magicLoadDefault,
                                                 magicOpen)
import           Network.HTTP.Types             (hContentType)
import           Network.HTTP.Types.Status      (status200)
import           Network.Wai                    (Response, rawPathInfo,
                                                 requestMethod, responseBuilder,
                                                 responseLBS)
import           Network.Wai.Handler.Warp       (runEnv)
import           System.Environment             (lookupEnv)
import           System.FilePath                (takeExtension)
import           Text.RE.Replace
import           Text.RE.TDFA.Text
import           Web.Fn
import           Web.Heroku                     (parseDatabaseUrl)
import qualified Web.Larceny                    as L

import qualified Shoebox.Blob.Box               as Box
import           Shoebox.Blob.Delete
import qualified Shoebox.Blob.Email             as Email
import qualified Shoebox.Blob.File              as File
import           Shoebox.BlobServer
import           Shoebox.BlobServer.Directory
import           Shoebox.BlobServer.Memory
import           Shoebox.Deletion
import           Shoebox.Images
import           Shoebox.Importer
import           Shoebox.Indexer
import           Shoebox.IndexServer
import           Shoebox.IndexServer.Postgresql
import           Shoebox.IndexServer.Sqlite
import           Shoebox.Items
import           Shoebox.Types
import           Shoebox.Util


type Fill = L.Fill ()
type Library = L.Library ()
type Substitutions = L.Substitutions ()

data Ctxt = Ctxt { _edit    :: Bool
                 , _req     :: FnRequest
                 , _store   :: SomeBlobServer
                 , _db      :: SomeIndexServer
                 , _library :: Library
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
                    Just pth'' -> return (SomeBlobServer (FileStore pth''), pth'')
                    Nothing    -> do
                      ht <- H.new
                      return (SomeBlobServer (MemoryStore ht), ":memory:")
  db_url <- fmap parseDatabaseUrl <$> lookupEnv "DATABASE_URL"
  idx' <- fmap T.pack <$> lookupEnv "INDEX"
  delete store
  (serv, nm) <- case (db_url, idx') of
                  (Just ps,_) -> do
                    c <- PG.connectPostgreSQL $ T.encodeUtf8 $ T.intercalate " " $ map (\(k,v) -> k <> "=" <> v) ps
                    return (SomeIndexServer (PG c), fromJust (lookup "dbname" ps))
                  (Nothing, Just idx) -> do
                    do c <- PG.connectPostgreSQL $ T.encodeUtf8 $ "dbname='" <> idx <> "'"
                       return (SomeIndexServer (PG c), idx)
                  (_, _) -> do sql <- readFile "migrations/sqlite.sql"
                               c <- SQLITE.open ":memory:" --"tmp.db"
                               let stmts = map T.unpack $ T.splitOn "\n\n" (T.pack sql)
                               mapM_ (\stmt -> SQLITE.execute_ c (fromString stmt)) stmts
                               let serv = SomeIndexServer (SL c)
                               log' "Running indexer to populate :memory: index."
                               -- NOTE(dbp 2017-05-29): Run many times because
                               -- we need permanodes in DB before files stored
                               -- in them are indexed
                               index store serv
                               index store serv
                               index store serv
                               return (serv, ":memory:")
  public <- fmap (fromMaybe "0") $ lookupEnv "PUBLIC"
  let edit = public == "0"
  log' $ "Opening " <> (if edit then "~PRIVATE~" else "~PUBLIC~") <> " Shoebox [Blobs " <> pth <> " Index " <> nm <> "]"
  return (Ctxt edit defaultFnRequest store serv lib)

main :: IO ()
main = withStderrLogging $
  do ctxt <- initializer
     runEnv 3000 $ toWAI ctxt site

instance FromParam SHA224 where
  fromParam [x] | "sha224-" `T.isPrefixOf` x = Right $ SHA224 x
  fromParam []  = Left ParamMissing
  fromParam _   = Left ParamTooMany

editable :: Ctxt -> Req -> IO (Maybe (Req, a -> a))
editable ctxt req = return $ if _edit ctxt then Just (req, id) else Nothing

site :: Ctxt -> IO Response
site ctxt = do
  log' $ T.decodeUtf8 (requestMethod (fst $ _req ctxt)) <> " " <> T.decodeUtf8 (rawPathInfo (fst $ _req ctxt))
  route ctxt [ end // param "page" // editable ctxt ==> indexH
             , path "static" ==> staticServe "static"
             , path "new" // param "title" // editable ctxt ==> newBoxH
             , segment // path "thumb" ==> thumbH
             , segment // path "delete" // editable ctxt ==> deleteH
             , segment // path "remove" // segment // editable ctxt ==> boxDeleteH
             , segment ==> renderH
             , segment ==> redirectH
             , path "blob" // segment ==> blobH
             , path "file" // segment ==> \ctxt sha -> File.serve (_store ctxt) sha
             , path "raw" // segment ==> rawH
             , path "upload" // param "box" // file "file" !=> uploadH
             , path "search" // param "q" // editable ctxt ==> searchH
             , path "reindex" ==> reindexH
             , path "wipe" ==> wipeH
             ]
    `fallthrough` do r <- render ctxt "404"
                     case r of
                       Just r' -> return r'
                       Nothing -> notFoundText "Page not found"

indexH :: Ctxt -> Maybe Int -> IO (Maybe Response)
indexH ctxt page = do
  is <- getItems (_db ctxt) (fromMaybe 0 page)
  renderWith ctxt
    (L.subs [("has-more", L.fillChildren)
            ,("next-page", L.textFill $ maybe "1" (T.pack . show . (+1)) page)
            ,("items", L.mapSubs itemSubs is)
            ,("q", L.textFill "")])
    "index"

searchH :: Ctxt -> Text -> IO (Maybe Response)
searchH ctxt q = do
  if T.strip q == "" then redirect "/" else do
    is <- search (_db ctxt) q
    if length is == 0 then redirect "/" else
      renderWith ctxt
        (L.subs [("has-more", L.textFill "")
                ,("q", L.textFill q)
                ,("items", L.mapSubs itemSubs is)])
        "index"

reindexH :: Ctxt -> IO (Maybe Response)
reindexH ctxt = do
  delete (_store ctxt)
  index (_store ctxt) (_db ctxt)
  okText "OK."

wipeH :: Ctxt -> IO (Maybe Response)
wipeH ctxt = do
  wipe (_db ctxt)
  redirect "/"

mmsum :: (Monad f, MonadPlus m, Foldable t) => t (f (m a)) -> f (m a)
mmsum = foldl (liftA2 mplus) (return mzero)

renderH :: Ctxt -> SHA224 -> IO (Maybe Response)
renderH ctxt sha = do
  res' <- readBlob (_store ctxt) sha
  case res' of
    Nothing  -> return Nothing
    Just bs ->
      liftA2 mplus
      (mmsum $ map (\f -> f (_store ctxt) (_db ctxt) (renderWith ctxt) sha bs)
        [Box.toHtml
        ,File.toHtml
        ,Email.toHtml
        ])
      (blobH ctxt sha)

newBoxH :: Ctxt -> Text -> IO (Maybe Response)
newBoxH ctxt title = do
  rand <- Box.mkRandom
  let box = Box.BoxBlob rand title [] Nothing
  (SHA224 ref) <- writeBlob (_store ctxt) (BL.toStrict $ encodePretty box)
  Box.indexBlob (_store ctxt) (_db ctxt) (SHA224 ref) box
  redirect $ "/" <> ref

blobH :: Ctxt -> SHA224 -> IO (Maybe Response)
blobH ctxt sha = do
  res' <- readBlob (_store ctxt) sha
  case res' of
    Nothing  -> return Nothing
    Just bs ->
      route ctxt [anything ==> \_ -> do
                     m <- magicOpen [MagicMimeType]
                     magicLoadDefault m
                     let b = BL.toStrict bs
                     mime <- unsafeUseAsCStringLen b (magicCString m)
                     let display = renderWith ctxt (L.subs [("content", L.rawTextFill (hyperLinkEscape (T.decodeUtf8 b)))]) "blob"
                     case mime of
                       "text/plain" -> display
                       "text/html"  -> display
                       _            -> rawH ctxt sha]

rawH :: Ctxt -> SHA224 -> IO (Maybe Response)
rawH ctxt sha@(SHA224 s) =
  do res' <- readBlob (_store ctxt) sha
     case res' of
       Nothing  -> return Nothing
       Just res -> return $ Just $ responseLBS status200 [] res

renderIcon :: IO (Maybe Response)
renderIcon = sendFile "static/icon.png"

thumbH :: Ctxt -> SHA224 -> IO (Maybe Response)
thumbH ctxt sha =
  do res <- getThumbnail (_db ctxt) sha
     case res of
       Nothing -> renderIcon
       Just jpg -> return $ Just $ responseBuilder status200 [(hContentType, "image/jpeg")] (Builder.fromByteString jpg)

deleteH :: Ctxt -> SHA224 -> IO (Maybe Response)
deleteH ctxt sha =
  do now <- getCurrentTime
     shas <- findConnectedBlobs (_store ctxt) sha
     mapM (\s -> writeBlob (_store ctxt) (BL.toStrict $ encodePretty (DeleteBlob s now))) shas
     delete (_store ctxt)
     wipe (_db ctxt)
     index (_store ctxt) (_db ctxt)
     redirectReferer ctxt

boxDeleteH :: Ctxt -> SHA224 -> SHA224 -> IO (Maybe Response)
boxDeleteH ctxt boxSha eltSha =
  do b <- readBlob (_store ctxt) boxSha
     case decode =<< b of
       Nothing -> return Nothing
       Just (Box.BoxBlob r t contents p') ->
         if eltSha `elem` contents
         then do let p = if p' == Just eltSha then Nothing else p'
                 let newbox = Box.BoxBlob r t (filter (/= eltSha) contents) p
                 Box.updateBox (_store ctxt) (_db ctxt) boxSha newbox
                 redirectReferer ctxt
         else redirectReferer ctxt

uploadH :: Ctxt -> Maybe SHA224 -> File -> IO (Maybe Response)
uploadH ctxt boxRef f = do
  case boxRef of
    Nothing ->
      log' $ "Uploading " <> fileName f <> "..."
    Just (SHA224 box) ->
      log' $ "Uploading " <> fileName f <> " to box " <> box <> "..."
  process (_store ctxt) (_db ctxt) boxRef f
  okText "OK"

redirectH :: Ctxt -> SHA224 -> IO (Maybe Response)
redirectH ctxt sha = do red <- getRedirections (_db ctxt) sha
                        is <- fmap catMaybes $ mapM (getItem (_db ctxt)) red
                        case is of
                          [(Item (SHA224 target) _ _)] -> redirect ("/" <> target)
                          [] -> return Nothing
                          is -> do
                            renderWith ctxt (L.subs [("has-more", L.textFill "")
                                                    ,("q", L.textFill "")
                                                    ,("items", L.mapSubs itemSubs is)])
                                       "index"
