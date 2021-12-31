#include <stdlib.h>

#include <lib/gfx/libgfx.h>
#include <lib/gui/obj.h>
#include <lib/gui/frame.h>
#include <lib/gui/layout-ops.h>
#include <lib/gui/message.h>

void __fastcall__ draw_frame (struct obj *);

struct obj_ops frame_ops = {
    draw_frame,
    layout_none,
    obj_noop
};

struct obj *
make_frame ()
{
    struct obj * f = alloc_obj (sizeof (struct obj), &frame_ops);
    return f;
}

void __fastcall__
draw_frame (struct obj * f)
{
    gfx_push_context ();
    set_obj_region (f);
    draw_obj_children (f);
    gfx_pop_context ();
}
