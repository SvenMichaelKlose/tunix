#include <stdlib.h>
#include <stdio.h>

#include "libgfx.h"
#include "obj.h"
#include "button.h"
#include "error.h"
#include "layout-ops.h"
#include "list.h"
#include "table.h"
#include "window.h"

struct window * wleft;
struct window * wright;

void
init_filewindow (struct window * w, char device)
{

    /* Make window. */
    /* Draw window. */
    /* Load directory listing. */
    /* Draw window content. */

	wleft = make_window (0, 0, 81, 176, "#8");
	draw_window (wleft);
    wright = make_window (80, 0, 80, 176, "#1");
    draw_window (wright);
}

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
    char * msg = malloc (256);
    struct obj * o;
	struct list * hlist;
	struct button * b_ok;
	struct button * b_cancel;
	struct window * win = make_window (20, 22, 120, 110, "Start BASIC...");
	struct obj * table = make_table (0, 0, 0, 0);

    set_obj_ops (table, &table_ops_center);

    append_obj (OBJ(win), OBJ(table));

	hlist = make_list (0, 0, 60, 12, LIST_HORIZONTAL);
	b_ok = make_button (0, 0, 30, 12, "OK");
    b_cancel = make_button (0, 0, 30, 12, "Cancel");
    append_obj (OBJ(table), OBJ(hlist));
    append_obj (OBJ(hlist), OBJ(b_ok));
    append_obj (OBJ(hlist), OBJ(b_cancel));

    hlist = make_list (0, 0, 60, 12, LIST_HORIZONTAL);
    b_ok = make_button (0, 0, 30, 12, "Perhaps this works");
    b_cancel = make_button (0, 0, 30, 12, "Panic");
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
