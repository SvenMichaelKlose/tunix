#include "g.h"

#include <stdlib.h>
#include <stdio.h>

#include <conio.h>

#include <libgfx.h>

#include "obj.h"
#include "event.h"
#include "box.h"
#include "button.h"
#include "cbm.h"
#include "error.h"
#include "frame.h"
#include "layout-ops.h"
#include "list.h"
#include "message.h"
#include "table.h"
#include "window.h"

#include "bank-allocator.h"
#include "basic-starter.h"
#include "file-window.h"
#include "main.h"
#include "ultimem.h"

struct obj * desktop;
struct obj * focussed_window = NULL;

void
shift_charset ()
{
    int i;

    for (i = 0; i < 2048; i++)
        charset_4x8[i] <<= 4;
}

char do_shutdown = 0;

void
init ()
{
    /* Active RAM in BANK5. */
    * (char *) 0x9ff2 = 0xff;

    /* Add memory blocks for malloc(). */
    _heapadd ((void *) 0xa000, 0x2000);  /* BANK5 */
    _heapadd ((void *) 0x400, 0xc00);    /* +3K */
    _heapadd ((void *) 0x9800, 0x7f0);   /* IO2/3 excluding Ultimem registers. */

    init_bank_allocator ();
    gfx_clear_screen (0);
    gfx_init ();
    shift_charset ();
    gfx_set_font (charset_4x8, 2);

    focussed_window = NULL;
    desktop = OBJ(make_box (pattern_woven));
    set_obj_position_and_size (desktop, 0, 0, 20 * 8, 12 * 16 - MESSAGE_HEIGHT);
}

void
show_free_memory ()
{
    char * msg = malloc (64);
    sprintf (msg, "%U/%U B free.", _heapmemavail (), _heapmaxavail ());
    print_message (msg);
    free (msg);
}

//char buf[64];

#define DESKTOP_HEIGHT  (12 * 16 - MESSAGE_HEIGHT)

int
main (int argc, char ** argv)
{
    char key;
    struct event * e;
    struct window * w;
    unsigned idle;

    init ();
    show_free_memory ();
    append_obj (desktop, make_file_window ("#31 Ultimem", 0, 11 * 8, 20 * 8, 11 * 8));
    append_obj (desktop, make_file_window ("#8 SD2IEC", 0, 0, 20 * 8, 11 * 8));

    layout_obj (desktop);
    draw_obj (desktop);

    focussed_window = desktop->node.children->node.next;
    do {
        idle = 0;
        while (!(key = cbm_k_getin ()))
            if (!++idle)
                show_free_memory ();
//        sprintf (buf, "Key code %u", key);
//        print_message (buf);

        switch (key) {
            case 'F':
                w = (struct window *) focussed_window;
                w->flags ^= W_FULLSCREEN;
                if (w->flags & W_FULLSCREEN)
                    set_obj_position_and_size (focussed_window, 0, 0, 20 * 8, DESKTOP_HEIGHT);
                else
                    set_obj_position_and_size (focussed_window, 0, 0, 20 * 8, DESKTOP_HEIGHT / 2);
                layout_obj (desktop);
                draw_obj (desktop);
                show_free_memory ();
                continue;
        }

        /* Send keyboard event. */
        e = malloc (sizeof (struct event));
        e->type = EVT_KEYPRESS;
        e->data_char = key;
        send_event ((struct obj *) focussed_window, e);
        free (e);
    } while (!do_shutdown);

    return 0;
}
