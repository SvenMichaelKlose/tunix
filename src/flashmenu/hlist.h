#ifndef HLIST_H
#define HLIST_H

#include "obj.h"

extern struct obj_ops hlist_ops;

struct hlist {
    struct obj  obj;
};

extern struct hlist * __fastcall__ make_hlist (gpos x, gpos y, gsize w, gsize h);
extern void __fastcall__ draw_hlist (void *);

#endif /* #ifndef HLIST_H */
