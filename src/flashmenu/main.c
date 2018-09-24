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

void __fastcall__
desktop_draw (struct obj * o)
{
    struct window * w = (struct window *) o->node.children;

    while (w) {
        if (w->flags & W_FULLSCREEN) {
            draw_obj ((struct obj *) w);
            return;
        }
        w = (struct window *) w->obj.node.next;
    }

    draw_box (o);
}

struct obj_ops desktop_obj_ops = {
    desktop_draw,
    layout_obj_children,
    obj_noop,
    event_handler_passthrough
};

void
init ()
{
    /* Active RAM in BANK5. */
    * (char *) 0x9ff2 = 0xff;
    *ULTIMEM_BLK5 = *ULTIMEM_BLK3 + 1;

    /* Add memory blocks for malloc(). */
    _heapadd ((void *) 0xa000, 0x2000);  /* BANK5 */
    _heapadd ((void *) 0x400, 0xc00);    /* +3K */
    _heapadd ((void *) 0x9800, 0x7f0);   /* IO2/3 excluding Ultimem registers. */

    gfx_clear_screen (0);
    gfx_init ();
    shift_charset ();
    gfx_set_font (charset_4x8, 2);

    focussed_window = NULL;
    desktop = OBJ(make_box (pattern_woven));
    desktop->ops = &desktop_obj_ops;
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
    append_obj (desktop, make_file_window (&ultifs_drive_ops, "Ultimem ROM", 0, 0, 20 * 8, DESKTOP_HEIGHT / 2));
    append_obj (desktop, make_file_window (&cbm_drive_ops, "#8", 0, DESKTOP_HEIGHT / 2, 20 * 8, DESKTOP_HEIGHT / 2));

    focussed_window = desktop->node.children;
//    ((struct window *) focussed_window)->flags |= W_FULLSCREEN;
    layout_obj (desktop);
    draw_obj (desktop);

    do {
        idle = 0;
        while (!(key = cbm_k_getin ()));
//        sprintf (message_buffer, "Key code %U", key);
//        print_message (message_buffer);

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
                continue;

            case 'M':
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
