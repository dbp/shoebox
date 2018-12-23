{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Shoebox.Images where

import           Codec.Picture          (DynamicImage (ImageRGB8), Image (..),
                                         convertRGB8, decodeImage)
import           Codec.Picture.Extra    (scaleBilinear)
import           Codec.Picture.Saving   (imageToJpg)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Lazy   as BL
import           Data.ByteString.Unsafe (unsafePackMallocCStringLen)
import           Data.Monoid            ((<>))
import           Foreign.C.Types
import qualified Language.C.Inline      as C

C.context (C.baseCtx <> C.bsCtx)
C.include "<stdlib.h>"
C.include "<string.h>"
C.include "<libexif/exif-loader.h>"

getExifThumbnail :: ByteString -> IO (Maybe ByteString)
getExifThumbnail jpg = (C.withPtr $ \str -> C.withPtr $ \size -> [C.block|
    int {
      ExifLoader *l = exif_loader_new();
      ExifData *ed;
      exif_loader_write(l, (unsigned char *)$bs-ptr:jpg, $bs-len:jpg);
      ed = exif_loader_get_data(l);
      exif_loader_unref(l);
      if (ed) {
        if (ed->data && ed->size) {
          *$(char** str) = (char *)malloc(ed->size);
          memcpy(*$(char** str), ed->data, ed->size);
          *$(int* size) = ed->size;
          exif_data_unref(ed);
          return 0;
        } else {
          exif_data_unref(ed);
          return -1;
        }
      } else {
        return -1;
      }
    }
  |]) >>= \(ptr, (len, rv)) ->
   if rv == 0 then do
     Just <$> unsafePackMallocCStringLen (ptr, fromIntegral len)
   else
     return Nothing

createThumbnail :: ByteString -> IO (Maybe BL.ByteString)
createThumbnail = createSized 256

createMedium :: ByteString -> IO (Maybe BL.ByteString)
createMedium = createSized 1024

createSized :: Int -> ByteString -> IO (Maybe BL.ByteString)
createSized size bs =
  case decodeImage bs of
    Left _    -> return Nothing
    Right img' ->
      do let img = convertRGB8 img'
         let ht = imageHeight img
         let wd = imageWidth img
         let (wd', ht') = if ht >= wd then
                           ((size * wd) `div` ht, size)
                         else
                           (size, (size * ht) `div` wd)
         return $ Just $ imageToJpg 95 $ ImageRGB8 $ scaleBilinear wd' ht' img
