#include <stdlib.h>

#include <lib/gfx/libgfx.h>
#include <lib/gui/event.h>
#include <lib/gui/obj.h>
#include <lib/gui/message.h>
#include <lib/gui/list.h>
#include <lib/gui/layout-ops.h>

void __fastcall__ layout_list (struct obj *);

struct obj_ops list_ops = {
    draw_list,
    layout_list,
    obj_noop
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
    struct list * l = LIST(o);
    struct obj * last_child;
    gpos x = 0;
    gpos y = 0;
    char orientation = l->orientation;

    o = o->node.children;
    while (o) {
        set_obj_position (o, x, y);
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
    set_obj_size (OBJ(l), x, y);
}

void __fastcall__
draw_list (struct obj * o)
{
    gfx_push_context ();
    set_obj_region (o);
    draw_obj_children (o);
    gfx_pop_context ();
}
