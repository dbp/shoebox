{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Control.Logging
import Network.HTTP.Types.Status (status200)
import Network.Wai.Handler.Warp (runEnv)
import Network.Wai (Response, responseLBS)
import Web.Fn
import qualified Data.Text as T

import Database.Shed.Types
import Database.Shed.BlobServer
import Database.Shed.BlobServer.Directory

data Ctxt = Ctxt { _req :: FnRequest
                 , _store :: FileStore }
instance RequestContext Ctxt where
  getRequest = _req
  setRequest c r = c { _req = r }

initializer :: IO Ctxt
initializer = return (Ctxt defaultFnRequest (FileStore "."))

main :: IO ()
main = withStderrLogging $
  do ctxt <- initializer
     log' "Opening up the shed..."
     runEnv 3000 $ toWAI ctxt site

instance FromParam SHA1 where
  fromParam [x] | "sha1-" `T.isPrefixOf` x = Right $ SHA1 x
  fromParam []             = Left ParamMissing
  fromParam _              = Left ParamTooMany

site :: Ctxt -> IO Response
site ctxt = route ctxt [ segment ==> blobH
                       ]
                  `fallthrough` notFoundText "Page not found."

blobH :: Ctxt -> SHA1 -> IO (Maybe Response)
blobH ctxt sha@(SHA1 s) =
  do log' $ "Reading " <> s
     res' <- readBlob (_store ctxt) sha
     case res' of
       Nothing -> return Nothing
       Just res -> return $ Just $ responseLBS status200 [] res
