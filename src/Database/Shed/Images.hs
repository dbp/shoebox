{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Database.Shed.Images where

import           Data.ByteString        (ByteString)
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
