#include <stdlib.h>
#include <stdio.h>

#include "libgfx.h"
#include "obj.h"
#include "button.h"
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
draw_list (gpos x, gpos y, char ** txts)
{
    while (*txts) {
        gfx_draw_text (x, y, *txts++);
        y += 10;
    }
}

void
win_basic_start ()
{
    gpos x = 20;
    char * buttons[] = {
        "( )",
        "[ ]",
        "( )",
        "( )",
        "( )",
        NULL
    };
    char * labels[] = {
        "no extra memory",
        "+3K",
        "+8K",
        "+16K",
        "+24K",
        NULL
    };
	struct window * win = make_window (20, 44, 120, 88, "Start BASIC...");
	struct button * b = make_button (50, 118, 20, 12, "OK");

	draw_window (win);
    gfx_set_pattern (pattern_solid);
    draw_list (45, 66, buttons);
    draw_list (59, 66, labels);
    draw_button (b);
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
    shift_charset ();
    gfx_init ();
    draw_background ();
    win_basic_start ();

    while (1);
    return 0;
}
