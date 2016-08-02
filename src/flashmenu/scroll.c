#include <stdlib.h>
#include <string.h>

#include "libgfx.h"
#include "obj.h"
#include "frame.h"
#include "layout-ops.h"
#include "error.h"
#include "bank-allocator.h"
#include "scroll.h"
#include "ultimem.h"

void __fastcall__ draw_scroll (struct obj *);
void __fastcall__ free_scroll (struct obj *);

struct obj_ops scroll_ops = {
    draw_scroll,
    layout_max_size,
    free_scroll
};

struct scroll *
make_scroll ()
{
    struct scroll * s = alloc_obj (sizeof (struct scroll), &scroll_ops);
    char bank = alloc_bank ();
    char old_blk5 = *ULTIMEM_BLK5;

    if (!bank)
        print_error ("Can't allocate bank for scroll.");

    // Clear scroll.
    s->bank = bank;
    *ULTIMEM_BLK5 = s->bank;
    bzero (0xa000, 0x2000);
    *ULTIMEM_BLK5 = old_blk5;

    return s;
}

void __fastcall__
draw_scroll (struct obj * s)
{
    struct scroll * scroll = (struct scroll *) s;
    char old_blk5 = *ULTIMEM_BLK5;

    gfx_push_context ();
    *ULTIMEM_BLK5 = scroll->bank;
    gfx_copy_area (0xa000, 0x1100, 0, 0, s->rect.x + gfx_rxl (), s->rect.y + gfx_ryt (), s->rect.w, s->rect.h);
    *ULTIMEM_BLK5 = old_blk5;
    gfx_pop_context ();
}

void __fastcall__
free_scroll (struct obj * s)
{
    free_bank (((struct scroll *) s)->bank);
}
