#include <stdlib.h>

#include "obj.h"
#include "button.h"
#include "layout-ops.h"
#include "list.h"
#include "table.h"
#include "window.h"
#include "ultimem.h"
#include "scrollable.h"

void
launch_file_window ()
{
	struct window * win = make_window ("#8");
	struct scrollable * scrollable = make_scrollable ();
    int i;

	set_obj_position_and_size (OBJ(win), 0, 0, 80, 12 * 16);

    append_obj (OBJ(win->obj.node.children), OBJ(scrollable));

    gfx_push_context ();
    *ULTIMEM_CONFIG2 = 0xff;
    gfx_set_screen_base (0xa000);
    gfx_set_position (0, 0);
    for (i = 0; i < 16; i++)
        gfx_putchar_fixed ('A');
    gfx_pop_context ();

    layout_obj (OBJ(win));
    draw_obj (OBJ(win));
}
