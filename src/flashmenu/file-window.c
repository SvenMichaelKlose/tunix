#include <stdlib.h>

#include "obj.h"
#include "button.h"
#include "layout-ops.h"
#include "list.h"
#include "table.h"
#include "window.h"

void
launch_file_window ()
{
	struct window * win = make_window ("Start BASIC...");
	struct list * vlist;
	struct button * b_ok;
	struct button * b_cancel;

	set_obj_position_and_size (OBJ(win), 0, 0, 80, 12 * 16);

	vlist = make_list (LIST_VERTICAL);
    append_obj (OBJ(win->obj.node.children), OBJ(vlist));

	b_ok = make_button ("Hotkey");
    b_cancel = make_button ("Type of expansion.");
    append_obj (OBJ(vlist), OBJ(b_ok));
    append_obj (OBJ(vlist), OBJ(b_cancel));

	b_ok = make_button ("0");
    b_cancel = make_button ("No memory expansion.");
    append_obj (OBJ(vlist), OBJ(b_ok));
    append_obj (OBJ(vlist), OBJ(b_cancel));

    b_ok = make_button ("1");
    b_cancel = make_button ("Toggle +3K.");
    append_obj (OBJ(vlist), OBJ(b_ok));
    append_obj (OBJ(vlist), OBJ(b_cancel));

    b_ok = make_button ("2");
    b_cancel = make_button ("+8K.");
    append_obj (OBJ(vlist), OBJ(b_ok));
    append_obj (OBJ(vlist), OBJ(b_cancel));

    b_ok = make_button ("3");
    b_cancel = make_button ("+16K.");
    append_obj (OBJ(vlist), OBJ(b_ok));
    append_obj (OBJ(vlist), OBJ(b_cancel));

    b_ok = make_button ("4");
    b_cancel = make_button ("+24K.");
    append_obj (OBJ(vlist), OBJ(b_ok));
    append_obj (OBJ(vlist), OBJ(b_cancel));

    b_ok = make_button ("5");
    b_cancel = make_button ("+32K.");
    append_obj (OBJ(vlist), OBJ(b_ok));
    append_obj (OBJ(vlist), OBJ(b_cancel));

    layout_obj (OBJ(win));
    draw_obj (OBJ(win));
}
