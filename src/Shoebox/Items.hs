{-# LANGUAGE OverloadedStrings #-}
module Shoebox.Items where

import           Data.Maybe        (isJust)
import qualified Data.Text         as T
import qualified HTMLEntities.Text as HE
import           Web.Larceny       (Substitutions)
import qualified Web.Larceny       as L

import           Shoebox.Types

itemSubs :: Item -> Substitutions ()
itemSubs (Item (SHA224 sha) thumb med prev) =
  L.subs [("contentRef", L.textFill sha)
         ,("has-thumbnail", justFill thumb)
         ,("no-thumbnail", nothingFill thumb)
         ,("has-medium", justFill med)
         ,("no-medium", nothingFill med)
         ,("has-preview", justFill prev)
         ,("preview", L.rawTextFill $ maybe "" (T.replace "\n" "</p><p>" . HE.text) prev)]
  where
    justFill m = if isJust m then L.fillChildren else L.textFill ""
    nothingFill m = if isJust m then L.textFill "" else L.fillChildren
