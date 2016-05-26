#include <stdlib.h>
#include <stdio.h>

#include "libgfx.h"
#include "obj.h"
#include "box.h"
#include "button.h"
#include "layout-ops.h"
#include "list.h"
#include "table.h"
#include "window.h"
#include "error.h"
#include "message.h"
#include "bank-allocator.h"
#include "basic-starter.h"
#include "file-window.h"
#include "main.h"

struct obj * desktop;

void
shift_charset ()
{
    int i;

    for (i = 0; i < 2048; i++)
        charset_4x8[i] <<= 4;
}

void
init_desktop ()
{
    init_bank_allocator ();
    gfx_init ();
    shift_charset ();
    gfx_set_font (charset_4x8, 2);

    desktop = OBJ(make_box (pattern_leaves));
    set_obj_position_and_size (desktop, 0, 0, 20 * 8, 12 * 16);
}

int
main (int argc, char ** argv)
{
    init_desktop ();

    append_obj (desktop, make_basic_starter ());
    append_obj (desktop, make_file_window ());

    layout_obj (desktop);
    draw_obj (desktop);

    print_message ("System halted. Please reset.");
    while (1);

    return 0;
}
