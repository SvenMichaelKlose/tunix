#include <stdlib.h>

#include "libgfx.h"
#include "obj.h"
#include "frame.h"
#include "layout-ops.h"
#include "message.h"

void __fastcall__ draw_frame (struct obj *);

struct obj_ops frame_ops = {
    draw_frame,
    layout_none
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
    gfx_push_region ();
    set_obj_region (f);
    draw_obj_children (f);
    gfx_pop_region ();
}
