#ifndef LIST_H
#define LIST_H

#include "obj.h"

extern struct obj_ops list_ops;

#define LIST_HORIZONTAL     0
#define LIST_VERTICAL       1

struct list {
    struct obj  obj;
    char        orientation;
};

struct list * __fastcall__ make_list (char orientation);
void __fastcall__ draw_list (void *);

#endif /* #ifndef LIST_H */
