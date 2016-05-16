#include <stdlib.h>
#include <stdio.h>

#include "libgfx.h"
#include "obj.h"
#include "button.h"
#include "layout-ops.h"
#include "list.h"
#include "table.h"
#include "window.h"

struct window * wleft;
struct window * wright;

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
win_basic_start ()
{
	struct list * hlist;
	struct button * b_ok;
	struct button * b_cancel;
	struct window * win = make_window ("Start BASIC...");
	struct obj * table = make_table (0);

	set_obj_position_and_size (OBJ(win), 20, 22, 120, 110);
    set_obj_ops (table, &table_ops_center);

    append_obj (OBJ(win), OBJ(table));

	hlist = make_list (LIST_HORIZONTAL);
	b_ok = make_button ("OK");
    b_cancel = make_button ("Cancel");
    append_obj (OBJ(table), OBJ(hlist));
    append_obj (OBJ(hlist), OBJ(b_ok));
    append_obj (OBJ(hlist), OBJ(b_cancel));

    hlist = make_list (LIST_HORIZONTAL);
    b_ok = make_button ("Perhaps this works");
    b_cancel = make_button ("Panic");
    append_obj (OBJ(table), OBJ(hlist));
    append_obj (OBJ(hlist), OBJ(b_ok));
    append_obj (OBJ(hlist), OBJ(b_cancel));

    layout_obj (OBJ(win));
    draw_obj (OBJ(win));
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
    gfx_init ();
    shift_charset ();

    copy_obj_ops (&table_ops_center, &table_ops);
    former_table_layout = table_ops_center.layout;
    table_ops_center.layout = layout_table_center;

    draw_background ();
    win_basic_start ();

    while (1);
    return 0;
}
