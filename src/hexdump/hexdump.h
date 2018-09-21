#ifndef HEXDUMP_H
#define HEXDUMP_H

#include "window.h"

struct hexdump_content {
    struct obj obj;
    char * data;
    int    len;
    int    pos;    /* User's position in list. */
};

struct obj * __fastcall__ make_hexdump (char * data, unsigned len, char * title, gpos x, gpos y, gsize w, gsize h);

#endif /* #ifndef HEXDUMP_H */
