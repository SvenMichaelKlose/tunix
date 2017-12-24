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

void __fastcall__
draw_file_window (struct obj * w)
{
    int i, j;

    window_ops.draw (w);
/*
    *ULTIMEM_CONFIG2 = 0xff;
    *ULTIMEM_BLK5 = dir_bank;
    bzero (0xa000, 0x2000);
    cbm_read_directory ("$", 8);

    gfx_push_context ();
    for (j = 0; j < 16; j++) {
        *ULTIMEM_BLK5 = dir_bank;
        memcpy (name, p, 16);
        p += 21;

        *ULTIMEM_BLK5 = scroll->bank;
        gfx_set_screen_base (0xa000);
        gfx_set_position (0, j * 8);
        for (i = 0; i < 16; i++)
            if (c = name[i])
                gfx_putchar_fixed (name[i]);
    }
    gfx_pop_context ();
    */
}

struct obj *__fastcall__
make_file_window (char * title, gpos x, gpos y, gpos w, gpos h)
{
	struct window * win = make_window (title);
	struct obj_ops * ops = malloc (sizeof (struct obj_ops));
    char name[16];

    copy_obj_ops (ops, &window_ops);
    ops->draw = draw_file_window;
    set_obj_ops (OBJ(win), ops);
	set_obj_position_and_size (OBJ(win), x, y, w, h);

    return OBJ(win);
}
