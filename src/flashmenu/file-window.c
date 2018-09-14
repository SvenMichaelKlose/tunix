#include <string.h>
#include <stdlib.h>

#include "bank-allocator.h"
#include "obj.h"
#include "button.h"
#include "layout-ops.h"
#include "list.h"
#include "table.h"
#include "window.h"
#include "ultimem.h"
#include "cbm.h"

char name[21];

void __fastcall__
draw_file_window (struct obj * w)
{
    int i, j;
    char c;

    window_ops.draw (w);

    cbm_opendir ("$", 8);
    gfx_push_context ();
    gfx_set_font (0x8000, 0);
    gfx_set_font_compression (0);
    for (j = 0; j < 21; j++) {
        cbm_readdir (name);
        if (!name[0])
            break;
        gfx_set_position (2, 12 + j * 8);
        gfx_putchar (name[16] + 192);
        gfx_putchar (32);
        for (i = 0; i < 16; i++)
            if (c = name[i])
                gfx_putchar (c + 192);
            else
                break;
    }
    cbm_closedir ();
    gfx_pop_context ();
}

struct obj *__fastcall__
make_file_window (char * title, gpos x, gpos y, gpos w, gpos h)
{
	struct window * win = make_window (title);
	struct obj_ops * ops = malloc (sizeof (struct obj_ops));

    copy_obj_ops (ops, &window_ops);
    ops->draw = draw_file_window;
    set_obj_ops (OBJ(win), ops);
	set_obj_position_and_size (OBJ(win), x, y, w, h);

    return OBJ(win);
}
