{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Logging                    (log', withStderrLogging)
import           Data.Aeson
import           Data.Maybe                         (catMaybes, fromMaybe)
import           Data.Monoid                        ((<>))
import qualified Data.Text                          as T
import           Database.PostgreSQL.Simple         (Connection, Only (..),
                                                     connectPostgreSQL, query_)
import           Network.HTTP.Types.Status          (status200)
import           Network.Wai                        (Response, responseLBS)
import           Network.Wai.Handler.Warp           (runEnv)
import           System.Environment                 (lookupEnv)
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
site ctxt = route ctxt [
  end ==> indexH
  , segment ==> blobH
                       ]
                  `fallthrough` notFoundText "Page not found."

indexH :: Ctxt -> IO (Maybe Response)
indexH ctxt = do
  rs <- query_ (_db ctxt) "SELECT attributes->>'camliContent' as sha1 FROM permanodes WHERE show_in_ui = true"
  fs <- catMaybes <$> mapM (\(Only sha) -> (>>= decode) <$> readBlob (_store ctxt) sha) rs
  okHtml $ "<doctype !html><html><body><ul>" <> T.concat (map render fs) <> "</ul></body></html>"
  where render (FileBlob name [Part (SHA1 sha) _]) =
          "<li><a href='/" <> sha <> "'>" <> name <> "</a></li>"
        render _ = "<li>Not a file</li>"

blobH :: Ctxt -> SHA1 -> IO (Maybe Response)
blobH ctxt sha@(SHA1 s) =
  do log' $ "Reading " <> s
     res' <- readBlob (_store ctxt) sha
     case res' of
       Nothing  -> return Nothing
       Just res -> return $ Just $ responseLBS status200 [] res
