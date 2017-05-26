{-# LANGUAGE OverloadedStrings #-}
module Database.Bin.Types where

import Data.Text (Text)

newtype SHA1 = SHA1 { unSHA1 :: Text }
