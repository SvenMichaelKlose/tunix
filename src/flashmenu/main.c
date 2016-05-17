#include <stdlib.h>
#include <stdio.h>

#include "libgfx.h"
#include "obj.h"
#include "button.h"
#include "layout-ops.h"
#include "list.h"
#include "table.h"
#include "window.h"
#include "error.h"
#include "bank-allocator.h"
#include "basic-starter.h"
#include "file-window.h"

void
draw_background ()
{
    gfx_reset_region ();
    gfx_set_pattern (pattern_gray);
    gfx_draw_box (0, 0, 20 * 8, 12 * 16);
}

struct obj_ops table_ops_center;
func_layout_t former_table_layout;

void __fastcall__
layout_table_center (struct obj * obj)
{
    former_table_layout (obj);
    layout_center (obj);
}

void
shift_charset ()
{
    int i;

    for (i = 0; i < 2048; i++)
        charset_4x8[i] <<= 4;
}

int
main (int argc, char ** argv)
{
    init_bank_allocator ();
    gfx_init ();
    shift_charset ();

    copy_obj_ops (&table_ops_center, &table_ops);
    former_table_layout = table_ops_center.layout;
    table_ops_center.layout = layout_table_center;

    draw_background ();

    launch_file_window ();
    while (1);

    print_error ("Program exited.");
    return 0;
}
