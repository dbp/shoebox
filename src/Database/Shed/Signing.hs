{-# LANGUAGE OverloadedStrings #-}
module Database.Shed.Signing where

import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as B
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           System.Process

import           Database.Shed.Types

-- | signJson takes a GPG key id, serialized JSON, returns signed json.
--
-- The input json will be trimmed of one trailing } and any whitespace
-- beyond. Then it will be signed with gpg (shelling out...), after which
-- we append ,"camliSig":"<S>"}\n where <S> is, on a single line, the ASCII
-- armored signature (without the header/footer)
signJson :: Key -> ByteString -> IO ByteString
signJson (Key k _) t = do
  let s = dropEnd 1 . fst $ B.breakEnd (== (toEnum . fromEnum $ '}')) t
  (Just stdin, Just stdout, _, ph) <- createProcess ((shell "gpg --detach-sign --local-user=E03CF9DE --armor -") { std_out = CreatePipe, std_in = CreatePipe})
  B.hPut stdin t
  waitForProcess ph
  signed <- B.hGetContents stdout
  let stripped = T.concat $ takeWhile (not . ("-----END PGP SIGNATURE-----" `T.isPrefixOf`)) $ drop 2 $ T.lines $ T.decodeUtf8 signed
  return $ s <> ",\"camliSig\":\"" <> T.encodeUtf8 stripped <> "\"}\n"

getPubKey :: Text -> IO ByteString
getPubKey keyid =
  T.encodeUtf8 . T.pack <$> readCreateProcess
             (shell $ T.unpack $ "gpg --export --armor " <> keyid) ""

dropEnd :: Int -> ByteString -> ByteString
dropEnd n bs = B.take (B.length bs - n) bs
