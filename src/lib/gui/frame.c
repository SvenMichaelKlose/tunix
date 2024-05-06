#include <stdlib.h>

#include <gfx/libgfx.h>
#include <gui/obj.h>
#include <gui/frame.h>
#include <gui/layout-ops.h>
#include <gui/message.h>

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
