#include <stdlib.h>
#include <string.h>

#include "obj.h"
#include "layout-ops.h"
#include "table.h"
#include "message.h"
#include "error.h"

void __fastcall__
layout_none (struct obj * x)
{
    layout_obj_children (x);
}

void __fastcall__
layout_max_size (struct obj * x)
{
    struct rect * prect = &x->node.parent->rect;

    set_obj_size (x, prect->w, prect->h);
}

void __fastcall__
layout_inside (struct obj * x)
{
    struct rect * prect = &x->node.parent->rect;

    set_obj_frame (x, 1, 1, prect->w - 7, prect->h - 2);
}

void __fastcall__
layout_center (struct obj * x)
{
    struct rect * prect = &x->node.parent->rect;

    set_obj_position (x, (prect->w - x->rect.w) / 2, (prect->h - x->rect.h) / 2 - 2);
}

