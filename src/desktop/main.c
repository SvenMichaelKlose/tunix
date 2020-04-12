#pragma code-name ("DESKTOP")

#include "cc65-charmap.h"

#include <cbm.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <conio.h>

#include <ultimem.h>
#include <libgfx.h>
#include <ingle.h>

#include "obj.h"
#include "event.h"
#include "box.h"
#include "button.h"
#include "error.h"
#include "frame.h"
#include "inputline.h"
#include "layout-ops.h"
#include "list.h"
#include "message.h"
#include "table.h"
#include "window.h"

#include "ultifs.h"
#include "wrap-ultifs.h"
#include "basic-starter.h"
#include "file-window.h"
#include "main.h"

#define DESKTOP_WIDTH  (20 * 8)
#define DESKTOP_HEIGHT  (12 * 16 - MESSAGE_HEIGHT)

struct obj * desktop;
struct obj * focussed_window;
char do_shutdown = 0;

void
shift_charset ()
{
    short old_bank = *ULTIMEM_BLK5;
    int i;
    char * charset = (char *) 0xa000;

    *ULTIMEM_BLK5 = FONT_BANK;
    for (i = 0; i < 2048; i++)
        charset[i] <<= 4;
    *ULTIMEM_BLK5 = old_bank;
}

void __fastcall__
desktop_draw (struct obj * o)
{
    struct window * w = WINDOW(o->node.children);

    /* Look up full-screen window and draw only that. */
    while (w) {
        if (w->flags & W_FULLSCREEN && OBJ(w) == focussed_window) {
            draw_obj (OBJ(w));
            return;
        }
        w = WINDOW(w->obj.node.next);
    }

    draw_box (o);
}

struct obj_ops desktop_obj_ops = {
    desktop_draw,
    layout_obj_children,
    obj_noop,
    event_handler_passthrough,
    DESKTOP_BANK,
    DESKTOP_BANK,
    DESKTOP_BANK,
    DESKTOP_BANK
};

void
show_free_memory ()
{
    sprintf (message_buffer, "%U/%UB RAM free.", _heapmemavail (), _heapmaxavail ());
    print_message (message_buffer);
}

void
toggle_fullscreen ()
{
    struct window * w = WINDOW(focussed_window);

    w->flags ^= W_FULLSCREEN;

    if (w->flags & W_FULLSCREEN) {
        set_obj_position_and_size (focussed_window, 0, 0, DESKTOP_WIDTH, DESKTOP_HEIGHT);
        draw_obj (focussed_window);
    } else {
        set_obj_position_and_size (focussed_window, w->user_x, w->user_y, w->user_w, w->user_h);
        draw_obj (desktop);
    }
}

void
focus_next_window ()
{
    struct obj * next = desktop->node.children;
    struct obj * f = focussed_window;

    if (!next || !next->node.next)
        return;

    desktop->node.children = next->node.next;
    f->node.next = next;
    next->node.next = NULL;
    next->node.flags &= ~OBJ_NODE_INVISIBLE;
    focussed_window = next;

    if (!(f->node.flags & OBJ_NODE_INVISIBLE))
        window_draw_title (WINDOW(f));
    draw_obj (focussed_window);
}

void
hide_windows ()
{
    struct obj * i = desktop->node.children;

    while (i) {
        i->node.flags |= OBJ_NODE_INVISIBLE;
        i = i->node.next;
    }

    draw_obj (desktop);
}

char key;

void
send_key_event ()
{
    struct event * e = malloc (sizeof (struct event));
    e->type = EVT_KEYPRESS;
    e->data_char = key;
    send_event (focussed_window, e);
    free (e);
}

int timer = -1;

void
wait_for_key ()
{
    while (!(key = cbm_k_getin ())) {
        send_queued_event ();

        if (++timer)
            continue;

        save_desktop_state ();
    }

    timer = -0x1000;
}

void
restart ()
{
    do {
        wait_for_key ();
        //sprintf (message_buffer, "Key code %U", key);
        //print_message (message_buffer);

        switch (key) {
            case 'F':
                toggle_fullscreen ();
                continue;

            case 'R':
                draw_obj (desktop);
                continue;

            case 'M':
                show_free_memory ();
                continue;

            case 'N':
                focus_next_window ();
                continue;

            case 'D':
                hide_windows ();
                continue;
        }

        send_key_event ();
    } while (!do_shutdown);
}

int
main (int argc, char ** argv)
{
    /* RAM in BANK5. */
    * (char *) 0x9ff2 = 0xff;
    *ULTIMEM_BLK5 = *ULTIMEM_BLK3 + 1;

    /* Add memory blocks for malloc(). */
    _heapadd ((void *) 0xa000, 0x2000);  /* BANK5 */
    _heapadd ((void *) 0x400, 0xc00);    /* +3K */

    /* Init display. */
    shift_charset ();
    gfx_clear_screen (0);
    gfx_init ();
    gfx_set_font (charset_4x8, 2, FONT_BANK);

    /* Create desktop as the root element. */
    desktop = OBJ(make_box (pattern_woven));
    desktop->ops = &desktop_obj_ops;
    set_obj_position_and_size (desktop, 0, 0, DESKTOP_WIDTH, DESKTOP_HEIGHT);

    /* Mount file systems. */
    if (w_ultifs_mount ())
        print_message ("UltiFS corrupt! Read-only.");

    append_obj (desktop, w_make_file_window (&cbm_drive_ops, "#8", 0, DESKTOP_HEIGHT / 2, DESKTOP_WIDTH, DESKTOP_HEIGHT / 2));
    focussed_window = append_obj (desktop, w_make_file_window (&ultifs_drive_ops, "Ultimem ROM", 0, 0, DESKTOP_WIDTH, DESKTOP_HEIGHT / 2));

    print_message ("Welcome to Ingle! Press 'H' for help.");
    layout_obj (desktop);
    draw_obj (desktop);

    restart ();
    return 0;
}
