#include <stdlib.h>

#include <lib/gui/obj.h>
#include <lib/gui/button.h>
#include <lib/gui/error.h>
#include <lib/gui/layout-ops.h>
#include <lib/gui/list.h>
#include <lib/gui/table.h>
#include <lib/gui/window.h>
#include <desktop/desktop.h>

#include "basic-starter.h"

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
	struct window * win = make_window ("Boot options", 0, 0);
	struct obj * table = make_table ();

	set_obj_position_and_size (OBJ(win), 20, 24, 120, 120);

    set_obj_ops (table, &table_ops_center);
    append_obj (win->obj.node.children, table);

	make_table_button (table, "Key", "Action");
	make_table_button (table, "0", "unexpanded");
    make_table_button (table, "1", "+3K");
    make_table_button (table, "2", "+8K");
    make_table_button (table, "3", "+16K");
    make_table_button (table, "4", "+24K");
    make_table_button (table, "5", "+32K");
    make_table_button (table, "6", "+2K (I/O)");

    return OBJ(win);
}
