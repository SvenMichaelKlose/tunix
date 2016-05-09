#include <stdlib.h>

#include "obj.h"
#include "window.h"
#include "libgfx.h"

typedef unsigned short ushort;
typedef unsigned char uchar;

#define WHITE   1

#define CONFIG_TYPE     1

void __fastcall__
free_obj (struct obj * x)
{
    struct obj * c;

    while (x) {
        c = x->node.children;
        if (c)
            free_obj (c);
        free (x);
        x = x->node.next;
    }
}

void __fastcall__
draw_obj (struct obj * x)
{
    while (x) {
        x->draw (x);
        x = x->node.next;
    }
}

void
draw_scrollable (struct scrollable * x)
{}

void
draw_message (char * txt)
{}

void probe_devices ()
{
    draw_message ("Probing devices...");
}

void seek_and_load_config ()
{
    draw_message ("Seeking config...");
}

short devices;
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
draw_list (short x, short y, char ** txts)
{
    while (*txts) {
        gfx_draw_text (x, y, *txts++);
        y += 10;
    }
}

void
win_basic_start ()
{
    short x = 20;
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

	draw_window (win);
    gfx_set_pattern (pattern_solid);
    draw_list (45, 66, buttons);
    draw_list (59, 66, labels);
}

extern char charset_4x8[2048];

int
main (int argc, char ** argv)
{
    gfx_init ();
    gfx_set_font (charset_4x8);
    draw_background ();
    win_basic_start ();

    while (1);
    probe_devices ();
    seek_and_load_config ();

    return 0;
}
