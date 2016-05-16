#include <stdlib.h>

#include "libgfx.h"
#include "obj.h"
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
draw_and_layout_list (struct obj * o, char do_layout)
{
    struct list * l = (struct list *) o;
    gpos x = 0;
    gpos y = 0;
    char orientation = l->orientation;

    gfx_push_region ();
    set_obj_region (OBJ(l));

    o = o->node.children;
    while (o) {
        o->rect.x = x;
        o->rect.y = y;
        if (do_layout)
            layout_obj (o);
        else
            draw_obj (o);
        if (orientation == LIST_HORIZONTAL)
            x += o->rect.w;
        else
            y += o->rect.h;
        o = o->node.next;
    }

    /* Get bottom right corner position. */
    if (orientation == LIST_HORIZONTAL)
        y += o->rect.h;
    else
        x += o->rect.w;

    /* Resize to corner. */
    l->obj.rect.w = x;
    l->obj.rect.h = y;

    gfx_pop_region ();
}

void __fastcall__
layout_list (struct obj * l)
{
    draw_and_layout_list (l, 1);
}

void __fastcall__
draw_list (void * l)
{
    draw_and_layout_list (OBJ(l), 0);
}
