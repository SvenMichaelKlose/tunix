#include <stdlib.h>
#include <string.h>

#include "obj.h"
#include "layout-ops.h"
#include "table.h"
#include "error.h"

void __fastcall__
layout_none (struct obj * x)
{
    layout_obj_children (x);
}

void __fastcall__
layout_center (struct obj * x)
{
    struct obj * parent = x->node.parent;
    if (!parent)
        print_error ("no kids");
        return;

    x->rect.x = parent->rect.x + (parent->rect.w - x->rect.w) / 2;
    x->rect.y = parent->rect.y + (parent->rect.h - x->rect.h) / 2;
}
