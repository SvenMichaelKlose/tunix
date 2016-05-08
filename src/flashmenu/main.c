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

void
init_filewindow (struct window * w, char device)
{
    /* Make window. */
    /* Draw window. */
    /* Load directory listing. */
    /* Draw window content. */
}

int
main (int argc, char ** argv)
{
	struct window * wleft;
	struct window * wright;

    gfx_init ();
    gfx_clear_screen (WHITE);
    gfx_reset_region ();
    gfx_set_pattern (pattern_gray);
    gfx_draw_box (0, 0, 20 * 8, 12 * 16);

	wleft = make_window (0, 0, 81, 176, "#8");
	draw_window (wleft);
	wright = make_window (80, 0, 80, 176, "#1");
	draw_window (wright);

	while (1);
    probe_devices ();
    seek_and_load_config ();

    return 0;
}
