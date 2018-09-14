#ifndef FILE_WINDOW_H
#define FILE_WINDOW_H

#include "window.h"

struct dirent {
    char            name[16];
    unsigned long   size;
    unsigned char   type;
    struct dirent * next;
};

struct file_window {
    struct window win;
    struct dirent * files;
};

struct obj * __fastcall__ make_file_window (char * title, gpos x, gpos y, gsize w, gsize h);

#endif /* #ifndef FILE_WINDOW_H */
