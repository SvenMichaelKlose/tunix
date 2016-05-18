#include <stdlib.h>

#include "libgfx.h"
#include "obj.h"
#include "message.h"
#include "list.h"
#include "layout-ops.h"

void __fastcall__ layout_list (struct obj *);

struct obj_ops list_ops = {
    draw_list,
    layout_list
};

struct list * __fastcall__
make_list (char orientation)
{
    struct list * list = alloc_obj (sizeof (struct list), &list_ops);
    list->orientation = orientation;
    return list;
}

void __fastcall__
layout_list (struct obj * o)
{
    struct list * l = (struct list *) o;
    struct obj * last_child;
    gpos x = 0;
    gpos y = 0;
    char orientation = l->orientation;

    o = o->node.children;
    while (o) {
        o->rect.x = x;
        o->rect.y = y;
        layout_obj (o);
        if (orientation == LIST_HORIZONTAL)
            x += o->rect.w;
        else
            y += o->rect.h;
        last_child = o;
        o = o->node.next;
    }

    if (orientation == LIST_HORIZONTAL)
        y += last_child->rect.h;
    else
        x += last_child->rect.w;

    /* Resize to corner. */
    l->obj.rect.w = x;
    l->obj.rect.h = y;
}

void __fastcall__
draw_list (struct obj * o)
{
    gfx_push_context ();
    set_obj_region (o);
    draw_obj_children (o);
    gfx_pop_context ();
}
