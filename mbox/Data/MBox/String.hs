
-----------------------------------------------------------------------------
{- |
Module      :  Data.MBox
Copyright   :  (c) Gershom Bazerman, 2009
License     :  BSD 3 Clause
Maintainer  :  gershomb@gmail.com
Stability   :  experimental

Reads and writes mboxrd files as per <http://www.qmail.org/man/man5/mbox.html>.

This parser is written to be a streaming parser. Given a lazy source of data and a streaming consumer, you should be able to analyze arbitrary mbox files in constant space.

-}
-------------------------------------------------------------------------

module Data.MBox.String (MBox, Message(..), Header, parseMBox, parseForward, parseDateHeader, showMessage, showMBox, getHeader, isID, isDate) where
import Prelude hiding (tail, init, last, minimum, maximum, foldr1, foldl1, (!!), read)
import Control.Arrow
import Data.List (isPrefixOf)
import Data.Char
import Data.Maybe
import Data.Time
import Safe
import qualified Data.Time.Locale.Compat as LC

type MBox = [Message]
data Message = Message {fromLine :: String, headers :: [Header], body :: String} deriving (Read, Show)
type Header = (String, String)

-- | Reads a date header as a UTCTime
parseDateHeader :: String -> Maybe UTCTime
parseDateHeader header = listToMaybe . catMaybes $ map tryParse formats where
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
    case drop 1 $ dropWhile (/= "-----Original Message-----") (lines b) of
      [] -> origMsg
      xs -> headDef origMsg . parseMBox . unlines $ f:xs

-- | Reads a string as an mbox file.
parseMBox :: String -> MBox
parseMBox = go . lines
    where
      go [] = []
      go (x:xs) = uncurry (:) . (readMsg x *** go) . break ("From " `isPrefixOf`) $ xs
      readMsg :: String -> [String] -> Message
      readMsg x xs = uncurry (Message x) . second (unlines . map unquoteFrom). readHeaders $ xs
      readHeaders :: [String] -> ([Header], [String])
      readHeaders [] = ([],[])
      readHeaders (x:xs)
          | null x || all isSpace x || not (any (==':') x) = ([],xs)
          | otherwise = first ((second (killSpace . sanHeader . (++ headerCont) . drop 1) . break (==':') $ x):) $ readHeaders xs'
            where (headerCont, xs') = first ((" " ++) . unlines . map killSpace) . break notCont $ xs
                  notCont [] = True
                  notCont (c:cs) = not (isSpace c) || (all isSpace cs)
      unquoteFrom :: String -> String
      unquoteFrom xs'@('>':xs) = if "From " `isPrefixOf` dropWhile (=='>') xs
                                   then xs
                                   else xs'
      unquoteFrom xs = xs

sanHeader :: String -> String
sanHeader = map (\x -> if x == '\n' then ' ' else x)

-- | Renders an MBox into a String
showMBox :: MBox -> String
showMBox = concatMap showMessage

-- | Renders an individual message into a String.
showMessage :: Message -> String
showMessage (Message f hs b) = unlines $ f : map (\(x,y) -> (x ++ ": " ++ y)) hs ++ ["\n"] ++ map unFrom (lines b)
    where unFrom x
             | isFrom x  = '>':x
             | otherwise = x
          isFrom x = "From " `isPrefixOf` dropWhile (=='>') x

killSpace :: String -> String
killSpace = dropWhile isSpace . dropEndWhile isSpace

dropEndWhile :: (a -> Bool) -> [a] -> [a]
dropEndWhile p = foldr (\x xs -> if p x && null xs then [] else x:xs) []


-- | Header accessors

isID :: Header -> Bool
isID (x, _y) = x == "Message-ID"

isDate :: Header -> Bool
isDate (x, _y) = x == "Date"

getHeader :: (Header -> Bool) -> Message -> [String]
getHeader p = map snd . filter p . headers
