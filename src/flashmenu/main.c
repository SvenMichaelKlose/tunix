#include <stdlib.h>
#include <stdio.h>

#include "libgfx.h"
#include "obj.h"
#include "box.h"
#include "button.h"
#include "frame.h"
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
#include "ultimem.h"

struct obj * desktop;

void
shift_charset ()
{
    int i;

    for (i = 0; i < 2048; i++)
        charset_4x8[i] <<= 4;
}

void
init ()
{
    shift_charset ();
    init_bank_allocator ();
    gfx_init ();
    gfx_set_font (charset_4x8, 2);

    desktop = OBJ(make_box (pattern_woven));
    set_obj_position_and_size (desktop, 0, 0, 20 * 8, 12 * 16 - MESSAGE_HEIGHT);
}

int
main (int argc, char ** argv)
{
    struct obj * tmp;
    init ();

    print_message ("Welcome!");
    append_obj (desktop, tmp = make_file_window ("#8", 0, 0, 81, 12 * 16 - MESSAGE_HEIGHT));
    append_obj (desktop, tmp = make_file_window ("#8", 80, 0, 80, 12 * 16 - MESSAGE_HEIGHT));
    append_obj (desktop, tmp = make_basic_starter ());
    layout_obj (desktop);
    draw_obj (desktop);

    while (1);

    return 0;
}
