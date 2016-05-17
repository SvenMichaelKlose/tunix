#include <stdlib.h>

#include "obj.h"
#include "button.h"
#include "layout-ops.h"
#include "list.h"
#include "table.h"
#include "window.h"
#include "scrollable.h"

void
launch_file_window ()
{
	struct window * win = make_window ("#8");
	struct scrollable * scrollable = make_scrollable ();

	set_obj_position_and_size (OBJ(win), 0, 0, 80, 12 * 16);

    append_obj (OBJ(win->obj.node.children), OBJ(scrollable));

    layout_obj (OBJ(win));
    draw_obj (OBJ(win));
}
