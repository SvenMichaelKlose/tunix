#include <stdlib.h>

#include "obj.h"
#include "layout-ops.h"

void
layout_none (struct obj * x)
{
    layout_obj_children (x);
}

void
layout_center (struct obj * x)
{
    struct obj * parent = x->node.parent;
    if (!parent)
        return;

    x->rect.x = parent->rect.x + (parent->rect.w - x->rect.w) / 2;
    x->rect.y = parent->rect.y + (parent->rect.h - x->rect.h) / 2;
}
