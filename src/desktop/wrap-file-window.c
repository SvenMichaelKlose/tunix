#include <cbm.h>

#include <ultimem/ultimem.h>
#include <gui/obj.h>

#include "file-window.h"

struct obj * __fastcall__ w_make_file_window (struct drive_ops * ops, char * title, gpos x, gpos y, gsize w, gsize h)
{
    unsigned oldbank = *ULTIMEM_BLK1;
    struct obj * r;

    *ULTIMEM_BLK1 = FILE_WINDOW_BANK;
    r = make_file_window (ops, title, x, y, w, h);
    *ULTIMEM_BLK1 = oldbank;

    return r;
}
