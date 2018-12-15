{-# LANGUAGE OverloadedStrings #-}
module Shoebox.Items where

import Web.Larceny (Substitutions)
import qualified Web.Larceny as L
import qualified HTMLEntities.Text           as HE
import qualified Data.Text as T
import Data.Maybe (isJust)

import Shoebox.Types

itemSubs :: Item -> Substitutions ()
itemSubs (Item (SHA224 sha) thumb prev) =
  L.subs [("contentRef", L.textFill sha)
         ,("has-thumbnail", justFill thumb)
         ,("no-thumbnail", nothingFill thumb)
         ,("has-preview", justFill prev)
         ,("preview", L.rawTextFill $ maybe "" (T.replace "\n" "</p><p>" . HE.text) prev)]
  where
    justFill m = if isJust m then L.fillChildren else L.textFill ""
    nothingFill m = if isJust m then L.textFill "" else L.fillChildren
