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
    struct obj * parent = x->node.parent;
    struct rect * rparent = &parent->rect;

    set_obj_size (x, rparent->w, rparent->h);
}

void __fastcall__
layout_center (struct obj * x)
{
    struct obj * parent = x->node.parent;

    if (!parent)
        print_error ("layout_center has no parent.");

    x->rect.x = (parent->rect.w - x->rect.w) / 2;
    x->rect.y = (parent->rect.h - x->rect.h) / 2;
}

