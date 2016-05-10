#ifndef LIST_H
#define LIST_H

#include "obj.h"

extern struct obj_ops list_ops;

#define LIST_HORIZONTAL     0
#define LIST_VERTICAL       0

struct list {
    struct obj  obj;
    char        orientation;
};

extern struct list * __fastcall__ make_list (gpos x, gpos y, gsize w, gsize h, char orientation);
extern void __fastcall__ draw_list (void *);

#endif /* #ifndef LIST_H */
