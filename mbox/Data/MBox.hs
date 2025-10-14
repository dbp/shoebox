{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
{- |
Module      :  Data.MBox
Copyright   :  (c) Gershom Bazerman, 2009; ported to Text by Alexander Jerneck, 2012
License     :  BSD 3 Clause
Maintainer  :  gershomb@gmail.com
Stability   :  experimental

Reads and writes mboxrd files as per <http://www.qmail.org/man/man5/mbox.html>.

This module uses Lazy Text pervasively, and should be able to operate as a streaming parser. That is to say, given a lazy stream of Text, and a streaming processing function, you should be able to analyze large mbox files in constant space.

-}
-------------------------------------------------------------------------

module Data.MBox (MBox, Message(..), Header, parseMBox, parseForward, parseDateHeader, showMessage, showMBox, getHeader, isID, isDate) where

import Prelude hiding (tail, init, last, minimum, maximum, foldr1, foldl1, (!!), read)
import Control.Arrow
import Data.Char
import Data.Maybe
import Data.Time
import Safe
import qualified Data.Text.Lazy as T
import qualified Data.Time.Locale.Compat as LC

type MBox = [Message]
data Message = Message {fromLine :: T.Text, headers :: [Header], body :: T.Text} deriving (Read, Show)
type Header = (T.Text, T.Text)

-- | Reads a date header as a UTCTime
parseDateHeader :: T.Text -> Maybe UTCTime
parseDateHeader txt = listToMaybe . catMaybes $ map tryParse formats where
  header = T.unpack txt
  tryParse f = parseTime LC.defaultTimeLocale f header
  formats =
    [ "%a, %_d %b %Y %T %z"
    , "%a, %_d %b %Y %T %Z"
    , "%a, %d %b %Y %T %z"
    , "%a, %d %b %Y %T %Z"
    , "%a, %_d %b %Y %T %z (%Z)"
    , "%a, %_d %b %Y %T %z (GMT%:-z)"
    , "%a, %_d %b %Y %T %z (UTC%:-z)"
    , "%a, %_d %b %Y %T %z (GMT%:z)"
    , "%a, %_d %b %Y %T %z (UTC%:z)"
    , "%A, %B %e, %Y %l:%M %p"
    , "%e %b %Y %T %z"
    ]

-- | Attempts to retrieve the contents of a forwarded message from an enclosing message.
parseForward :: Message -> Message
parseForward origMsg@(Message f _ b) =
    case drop 1 $ dropWhile (/= T.pack "-----Original Message-----") (T.lines b) of
      [] -> origMsg
      xs -> headDef origMsg . parseMBox . T.unlines $ f:xs

-- | Parses Text as an mbox file.
parseMBox :: T.Text -> MBox
parseMBox = go . T.lines
    where
      go [] = []
      go (x:xs) = uncurry (:) . (readMsg x *** go) . break ((T.pack "From ") `T.isPrefixOf`) $ xs
      readMsg :: T.Text -> [T.Text] -> Message
      readMsg x xs = uncurry (Message x) . second (T.unlines . map unquoteFrom). readHeaders $ xs
      readHeaders :: [T.Text] -> ([Header], [T.Text])
      readHeaders [] = ([],[])
      readHeaders (x:xs)
          | T.null x || T.all isSpace x || not (T.any (==':') x) = ([],xs)
          | otherwise = first ((second (T.strip . sanHeader . (`T.append` headerCont) . T.drop 1) . T.break (==':') $ x):) $ readHeaders xs'
          where (headerCont, xs') = first ((T.pack " " `T.append`) . T.unlines . map T.strip) . break notCont $ xs
                notCont :: T.Text -> Bool
                notCont s = doesNotStartSpace s || allSpace s
                allSpace = T.all isSpace
                doesNotStartSpace s = case T.length s of
                                        0 -> True
                                        _ -> not (isSpace $ T.head s)


      unquoteFrom :: T.Text -> T.Text
      unquoteFrom xs'@(T.stripPrefix (T.pack ">") -> Just suf) = if (T.pack "From ") `T.isPrefixOf` T.dropWhile (=='>') suf
                                                                 then suf
                                                                 else xs'
      unquoteFrom xs = xs

sanHeader :: T.Text -> T.Text
sanHeader = T.replace (T.pack "\n") (T.pack " ")

-- | Renders an MBox into Text
showMBox :: MBox -> T.Text
showMBox = T.concat . map showMessage

-- | Renders an individual message into Text.
showMessage :: Message -> T.Text
showMessage (Message f hs b) = T.unlines $ f : formatHeaders hs ++ [(T.pack "\n")] ++ formatBody b
                               where
                                 formatHeaders = map (\(x,y) -> x `T.append` (T.pack ": ") `T.append` y)
                                 formatBody = map unFrom . T.lines
                                 unFrom x
                                     | isFrom x = '>' `T.cons` x
                                     | otherwise = x
                                 isFrom x = (T.pack "From ") `T.isPrefixOf` T.dropWhile (=='>') x

-- | Return True if header is a Message-ID header.
isID :: Header -> Bool
isID (x, _) = x == T.pack "Message-ID"

-- | Return True if header is a Date header.
isDate :: Header -> Bool
isDate (x, _) = x == T.pack "Date"

-- | Return the values of headers for which predicate is True
getHeader :: (Header -> Bool) -> Message -> [T.Text]
getHeader predFunc = map snd . filter predFunc . headers
