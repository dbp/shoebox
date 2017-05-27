
#include <stdlib.h>

#include <string.h>

#include <libexif/exif-loader.h>

int inline_c_Database_Shed_Images_0_5d3f4b827f0184a20d6069421da4cfe4202cd4e7(char * jpg_inline_c_0, long jpg_inline_c_1, char ** str_inline_c_2, char ** str_inline_c_3, int * size_inline_c_4) {

      ExifLoader *l = exif_loader_new();
      ExifData *ed;
      exif_loader_write(l, (unsigned char *)jpg_inline_c_0, jpg_inline_c_1);
      ed = exif_loader_get_data(l);
      exif_loader_unref(l);
      if (ed) {
        if (ed->data && ed->size) {
          *str_inline_c_2 = (char *)malloc(ed->size);
          memcpy(*str_inline_c_3, ed->data, ed->size);
          *size_inline_c_4 = ed->size;
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

