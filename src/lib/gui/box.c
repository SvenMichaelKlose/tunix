#include <stdlib.h>

#include <lib/gfx/libgfx.h>
#include <lib/gui/obj.h>
#include <lib/gui/event.h>
#include <lib/gui/layout-ops.h>
#include <lib/gui/message.h>
#include <lib/gui/box.h>

void __fastcall__ draw_box (struct obj *);

struct obj_ops box_ops = {
    draw_box,
    layout_none,
    obj_noop,
    event_handler_passthrough
};

struct box * __fastcall__
make_box (char * pattern)
{
    struct box * box = alloc_obj (sizeof (struct box), &box_ops);
    box->pattern = pattern;
    return box;
}

void __fastcall__
draw_box (struct obj * o)
{
    struct box * b = BOX(o);
    struct rect * r = &o->rect;

    gfx_set_pencil_mode (1);
    gfx_set_pattern (b->pattern);
    gfx_draw_box (0, 0, r->w, r->h);
    draw_obj_children (o);
}
