#include <stdlib.h>
#include <stdio.h>

#include "libgfx.h"
#include "obj.h"
#include "button.h"
#include "hlist.h"
#include "error.h"
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

void
win_basic_start ()
{
    char * msg = malloc (256);
    struct obj * o;

	struct window * win = make_window (20, 22, 120, 110, "Start BASIC...");
	struct hlist * hlist = make_hlist (22, 34, 120, 110);
	struct button * b_ok = make_button (50, 108, 20, 12, "OK");
	struct button * b_cancel = make_button (90, 108, 20, 12, "Cancel");

    append_obj (OBJ(win), OBJ(hlist));
    append_obj (OBJ(hlist), OBJ(b_ok));
    append_obj (OBJ(hlist), OBJ(b_cancel));

    layout_obj ((struct obj *) win);
    draw_obj ((struct obj *) win);
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
    draw_background ();
    win_basic_start ();

    while (1);
    return 0;
}
