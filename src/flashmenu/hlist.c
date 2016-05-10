#include <stdlib.h>

#include "libgfx.h"
#include "obj.h"
#include "hlist.h"
#include "layout-ops.h"

struct obj_ops hlist_ops = {
    draw_hlist,
    layout_none
};

struct hlist * __fastcall__
make_hlist (gpos x, gpos y, gsize w, gsize h)
{
    struct hlist * o = alloc_obj (sizeof (struct hlist), x, y, w, h, &hlist_ops);
    return o;
}

void __fastcall__
draw_hlist (void * _o)
{
    struct obj * o = _o;
    struct rect * r = &o->rect;
    gpos x = r->x;
    gpos y = r->y;

    o = o->node.children;
    while (o) {
        o->rect.x = x;
        o->rect.y = y;
        draw_obj (o);
        x += o->rect.w;
        o = o->node.next;
    }
}
