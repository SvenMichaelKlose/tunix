#include <stdlib.h>

#include "libgfx.h"
#include "obj.h"
#include "frame.h"
#include "layout-ops.h"
#include "error.h"
#include "bank-allocator.h"
#include "scrollable.h"

void __fastcall__ draw_scrollable (struct obj *);

struct obj_ops scrollable_ops = {
    draw_scrollable,
    layout_max_size
};

struct scrollable *
make_scrollable ()
{
    struct scrollable * s = alloc_obj (sizeof (struct scrollable), &scrollable_ops);
    char bank = alloc_bank ();

    if (!bank)
        print_error ("Can't allocate bank for scrollable.");

    s->bank = bank;

    return s;
}

void __fastcall__
draw_scrollable (struct obj * s)
{
    gfx_push_context ();
    gfx_pop_context ();
}
