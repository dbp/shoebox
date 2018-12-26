{-# LANGUAGE OverloadedStrings #-}
module Shoebox.Items where

import           Control.Monad.Trans (liftIO)
import           Data.Maybe          (isJust)
import qualified Data.Text           as T
import qualified HTMLEntities.Text   as HE
import           Web.Larceny         (Substitutions)
import qualified Web.Larceny         as L

import           Shoebox.IndexServer
import           Shoebox.Types

itemSubs :: SomeIndexServer -> Item -> Substitutions ()
itemSubs serv (Item (SHA224 sha) thumb prev) =
  L.subs [("contentRef", L.textFill sha)
         ,("has-thumbnail", justFill thumb)
         ,("no-thumbnail", nothingFill thumb)
         ,("has-preview", justFill prev)
         ,("preview", L.rawTextFill $ maybe "" (T.replace "\n" "</p><p>" . HE.text) prev)
         ,("search-high", L.textFill' $ do txt <- liftIO $ getSearchHigh serv (SHA224 sha)
                                           case txt of
                                             Nothing -> return ""
                                             Just t  -> return t)
         ,("notes", L.textFill' $ do notes <- liftIO $ getNotes serv (SHA224 sha)
                                     case notes of
                                       [] -> return ""
                                       _ -> return (T.intercalate "\n\n" (map snd notes)))
         ]
  where
    justFill m = if isJust m then L.fillChildren else L.textFill ""
    nothingFill m = if isJust m then L.textFill "" else L.fillChildren
