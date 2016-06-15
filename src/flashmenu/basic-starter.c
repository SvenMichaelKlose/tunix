#include <stdlib.h>

#include "obj.h"
#include "button.h"
#include "layout-ops.h"
#include "list.h"
#include "table.h"
#include "window.h"
#include "main.h"
#include "basic-starter.h"
#include "error.h"

void
make_table_button (struct obj * table, char * cmd, char * descr)
{
	struct obj * hlist = OBJ(make_list (LIST_HORIZONTAL));
	struct obj * b_ok = OBJ(make_button (cmd));
	struct obj * b_cancel = OBJ(make_button (descr));
    append_obj (table, hlist);
    append_obj (hlist, b_ok);
    append_obj (hlist, b_cancel);
}

struct obj *
make_basic_starter ()
{
	struct window * win = make_window ("Start BASIC...");
	struct obj * table = make_table ();

	set_obj_position_and_size (OBJ(win), 20, 20, 120, 110);

    set_obj_ops (table, &table_ops_center);
    append_obj (OBJ(win->obj.node.children), OBJ(table));

	make_table_button (table, "Hotkey", "Type of expansion.");
	make_table_button (table, "0", "No memory expansion.");
    make_table_button (table, "1", "Toggle +3K.");
    make_table_button (table, "2", "+8K.");
    make_table_button (table, "3", "+16K.");
    make_table_button (table, "4", "+24K.");
    make_table_button (table, "5", "+32K.");

    return OBJ(win);
}
