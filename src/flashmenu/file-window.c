#include <stdlib.h>

#include "obj.h"
#include "button.h"
#include "layout-ops.h"
#include "list.h"
#include "table.h"
#include "window.h"
#include "ultimem.h"
#include "scroll.h"
#include "cbm.h"

struct obj *__fastcall__
make_file_window (char * title, gpos x, gpos y, gpos w, gpos h)
{
	struct window * win = make_window (title);
	struct scroll * scroll = make_scroll ();
    char old_blk5;
    int i;

	set_obj_position_and_size (OBJ(win), x, y, w, h);

    append_obj (OBJ(win->obj.node.children), OBJ(scroll));

    gfx_push_context ();
    *ULTIMEM_CONFIG2 = 0xff;
    *ULTIMEM_BLK5 = scroll->bank;
    gfx_set_screen_base (0xa000);
    gfx_set_position (0, 0);
    for (i = 0; i < 16; i++)
        gfx_putchar_fixed ('A');
    gfx_pop_context ();

    return OBJ(win);
}
