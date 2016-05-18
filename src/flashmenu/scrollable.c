#include <stdlib.h>

#include "libgfx.h"
#include "obj.h"
#include "frame.h"
#include "layout-ops.h"
#include "error.h"
#include "bank-allocator.h"
#include "scrollable.h"
#include "ultimem.h"

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
    char old_blk5 = *ULTIMEM_BLK5;
    gfx_push_context ();
    gfx_copy_area (0xa000, 0x1100, 0, 0, s->rect.x + gfx_rxl (), s->rect.y + gfx_ryt (), s->rect.w, s->rect.h);
    *ULTIMEM_BLK5 = old_blk5;
    gfx_pop_context ();
}
