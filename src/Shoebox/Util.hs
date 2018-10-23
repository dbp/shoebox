{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Shoebox.Util where

import qualified Data.ByteString.Lazy as BL
import           Data.Text            (Text)
import qualified HTMLEntities.Text    as HE
import           Network.HTTP.Types   (status200)
import           Network.Wai          (Response, responseLBS)
import           Text.RE.Replace
import           Text.RE.TDFA.Text


rawBlob :: BL.ByteString -> IO (Maybe Response)
rawBlob bs = return $ Just $ responseLBS status200 [] bs

hyperLinkEscape :: Text -> Text
hyperLinkEscape t =
  replaceAll "<a href=\"/blob/${sha}\">${sha}</a>" $
    (HE.text t) *=~ [re|${sha}(sha224-[0-9a-f]{56})|]
