#include <stdlib.h>

#include "obj.h"
#include "libgfx.h"

void __fastcall__
free_obj (struct obj * x)
{
    struct obj * c;

    while (x) {
        c = x->node.children;
        if (c)
            free_obj (c);
        free (x);
        x = x->node.next;
    }
}

void __fastcall__
draw_obj (struct obj * x)
{
    while (x) {
        x->draw (x);
        x = x->node.next;
    }
}
