#include <stdlib.h>

#include "obj.h"
#include "button.h"
#include "layout-ops.h"
#include "list.h"
#include "table.h"
#include "window.h"
#include "basic-starter.h"

void
launch_basic_starter ()
{
	struct window * win = make_window ("Start BASIC...");
	struct obj * table = make_table ();
	struct list * hlist;
	struct button * b_ok;
	struct button * b_cancel;

	set_obj_position_and_size (OBJ(win), 20, 20, 120, 110);
    set_obj_ops (table, &table_ops_center);

    append_obj (OBJ(win->obj.node.children), OBJ(table));

	hlist = make_list (LIST_HORIZONTAL);
	b_ok = make_button ("Hotkey");
    b_cancel = make_button ("Type of expansion.");
    append_obj (OBJ(table), OBJ(hlist));
    append_obj (OBJ(hlist), OBJ(b_ok));
    append_obj (OBJ(hlist), OBJ(b_cancel));

	hlist = make_list (LIST_HORIZONTAL);
	b_ok = make_button ("0");
    b_cancel = make_button ("No memory expansion.");
    append_obj (OBJ(table), OBJ(hlist));
    append_obj (OBJ(hlist), OBJ(b_ok));
    append_obj (OBJ(hlist), OBJ(b_cancel));

    hlist = make_list (LIST_HORIZONTAL);
    b_ok = make_button ("1");
    b_cancel = make_button ("Toggle +3K.");
    append_obj (OBJ(table), OBJ(hlist));
    append_obj (OBJ(hlist), OBJ(b_ok));
    append_obj (OBJ(hlist), OBJ(b_cancel));

    hlist = make_list (LIST_HORIZONTAL);
    b_ok = make_button ("2");
    b_cancel = make_button ("+8K.");
    append_obj (OBJ(table), OBJ(hlist));
    append_obj (OBJ(hlist), OBJ(b_ok));
    append_obj (OBJ(hlist), OBJ(b_cancel));

    hlist = make_list (LIST_HORIZONTAL);
    b_ok = make_button ("3");
    b_cancel = make_button ("+16K.");
    append_obj (OBJ(table), OBJ(hlist));
    append_obj (OBJ(hlist), OBJ(b_ok));
    append_obj (OBJ(hlist), OBJ(b_cancel));

    hlist = make_list (LIST_HORIZONTAL);
    b_ok = make_button ("4");
    b_cancel = make_button ("+24K.");
    append_obj (OBJ(table), OBJ(hlist));
    append_obj (OBJ(hlist), OBJ(b_ok));
    append_obj (OBJ(hlist), OBJ(b_cancel));

    hlist = make_list (LIST_HORIZONTAL);
    b_ok = make_button ("5");
    b_cancel = make_button ("+32K.");
    append_obj (OBJ(table), OBJ(hlist));
    append_obj (OBJ(hlist), OBJ(b_ok));
    append_obj (OBJ(hlist), OBJ(b_cancel));

    layout_obj (OBJ(win));
    draw_obj (OBJ(win));
}
