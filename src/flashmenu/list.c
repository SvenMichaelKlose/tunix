#include <stdlib.h>

#include "libgfx.h"
#include "obj.h"
#include "list.h"
#include "layout-ops.h"

struct obj_ops list_ops = {
    draw_list,
    layout_none
};

struct list * __fastcall__
make_list (gpos x, gpos y, gsize w, gsize h, char orientation)
{
    struct list * list = alloc_obj (sizeof (struct list), x, y, w, h, &list_ops);
    list->orientation = orientation;
    return list;
}

void __fastcall__
draw_list (void * _l)
{
    struct list * l = _l;
    struct obj * o = (struct obj *) l;
    gpos x = 0;
    gpos y = 0;
    char orientation = l->orientation;

    o = o->node.children;
    while (o) {
        o->rect.x = x;
        o->rect.y = y;
        draw_obj (o);
        if (orientation == LIST_HORIZONTAL)
            x += o->rect.w;
        else
            y += o->rect.h;
        o = o->node.next;
    }
}
