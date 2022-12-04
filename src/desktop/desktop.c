#pragma code-name ("DESKTOP")

#include <lib/ingle/cc65-charmap.h>

#include <cbm.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <conio.h>

#include <lib/ingle/ingle.h>
#include <lib/ultimem/ultimem.h>
#include <lib/gfx/libgfx.h>
#include <lib/gui/obj.h>
#include <lib/gui/event.h>
#include <lib/gui/box.h>
#include <lib/gui/button.h>
#include <lib/gui/error.h>
#include <lib/gui/frame.h>
#include <lib/gui/inputline.h>
#include <lib/gui/layout-ops.h>
#include <lib/gui/list.h>
#include <lib/gui/message.h>
#include <lib/gui/table.h>
#include <lib/gui/window.h>

#include "file-window.h"
#include "desktop.h"

#define DESKTOP_WIDTH  (20 * 8)
#define DESKTOP_HEIGHT  (12 * 16 - MESSAGE_HEIGHT)

//
// Text viewer place here due to linker problems.
//

char txt_help[] =
    "INGLE is graphical user interface for the "
    "VIC with Ultimem expansion.  It provides "
    "a ROM file system with a collection of "
    "programs on it already.\n"
    "\n"
    "For more information please check \n"
    "http://sleepingelephant.com/denial/\n"
    "\n"
    "Desktop commands:\n"
    "\n"
    "N: Step to next window.\n"
    "F: Toggle full-screen window.\n"
    "D: Minimize all windows.\n"
    "R: Redraw all windows.\n"
    "M: Show free memory.\n"
    "\n"
    "File manager commands:\n"
    "\n"
    "Up/down: Step to next file.\n"
    "Enter: Launch file.\n"
    ;

struct textview_content {
    struct obj  obj;
    char        * data;
    char        * ptr;
    unsigned    size;
};

void __fastcall__
textview_draw_content (struct obj * w)
{
    struct window * win = WINDOW(w);
    struct textview_content * content = (struct textview_content *) w;
    char * ptr = content->ptr;
    unsigned y = 1;
    char c;

    gfx_push_context ();

    gfx_set_font (charset_4x8, 2, FONT_BANK);
    gfx_set_font_compression (1);
    gfx_set_pattern (pattern_empty);
    gfx_draw_box (0, 0, w->rect.w, w->rect.h);

    gfx_set_position (1, 1);
    while (*ptr) {
        c = *ptr++;
        if (c == 10)
            goto next;

        gfx_putchar (c);

        if (gfx_x () <= w->rect.w - 4)
            continue;

next:
        y += 10;
        while (*ptr == 10)
            ++ptr, y += 5;

        if (y > w->rect.h)
            break;

        gfx_set_position (1, y);

        while (*ptr == 32)
            ++ptr;
    }

    gfx_pop_context ();
}


void __fastcall__
textview_draw (struct obj * w)
{
    struct textview_content * content = (struct textview_content *) w;

    gfx_push_context ();
    gfx_reset_region ();
    set_obj_region (w);
    textview_draw_content (w);
    gfx_pop_context ();
}

void
textview_event_handler (struct obj * o, struct event * e)
{
    struct textview_content * content = (struct textview_content *) o->node.children;
    int visible_bytes = (content->obj.rect.h / 8) * 8;

    switch (e->data_char) {
        case CH_CURS_UP:
            content->ptr -= 1024;
            break;

        case CH_CURS_DOWN:
            content->ptr += 1024;
            break;
    }

    textview_draw ((struct obj *) content);
}

struct obj_ops obj_ops_textview_content = {
    textview_draw,
    obj_noop,
    obj_noop,
    event_handler_passthrough,
    DESKTOP_BANK,
    DESKTOP_BANK,
    DESKTOP_BANK,
    DESKTOP_BANK
};

struct textview_content *
make_textview_content ()
{
	struct obj * obj =  alloc_obj (sizeof (struct obj), &obj_ops_textview_content);
    struct textview_content * content = malloc (sizeof (struct textview_content));

    memcpy (content, obj, sizeof (struct obj));
    free (obj);

    return content;
}

struct obj * __fastcall__
make_textview (char * data, unsigned size, char * title, gpos x, gpos y, gpos w, gpos h)
{
    struct textview_content * content = make_textview_content ();
	struct window * win = make_window (title, (struct obj *) content, textview_event_handler);

    content->data = data;
    content->ptr = data;
    content->size = size;
	set_obj_position_and_size (OBJ(win), x, y, w, h);

    return OBJ(win);
}

//
// Desktop
//

struct obj * desktop;
struct obj * focussed_window;
char do_shutdown = 0;

void
draw_desktop (struct obj * o)
{
    struct obj * p = o->node.children;
    struct obj * fs = NULL;

    if (!p) {
        draw_box (o);
        return;
    }

    do {
        if (WINDOW(p)->flags & W_FULLSCREEN)
            fs = p;
    } while (p = p->node.next);

    if (!fs) {
        draw_box (o);
        return;
    }

    do {
        draw_obj (fs);
    } while (fs = fs->node.next);
}

struct obj_ops desktop_obj_ops = {
    draw_desktop,
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
        layout_obj (focussed_window);
        draw_obj (focussed_window);
    } else {
        set_obj_position_and_size (focussed_window, w->user_x, w->user_y, w->user_w, w->user_h);
        layout_obj (desktop);
        draw_obj (desktop);
    }
}

void
blur_windows ()
{
    struct obj * i = desktop->node.children;

    WINDOW(focussed_window)->flags &= ~W_HAS_FOCUS;
    if (focussed_window && !(focussed_window->node.flags & OBJ_NODE_INVISIBLE))
        window_draw_title (WINDOW(focussed_window));

    while (i) {
        WINDOW(i)->flags &= ~W_HAS_FOCUS;
        i = i->node.next;
    }
}

void
focus_next_window ()
{
    struct obj *     next = desktop->node.children;
    struct obj *     f = focussed_window;

    if (!next || !next->node.next)
        return;

    blur_windows ();

    desktop->node.children = next->node.next;
    f->node.next = next;
    next->node.next = NULL;
    next->node.flags &= ~OBJ_NODE_INVISIBLE;
    focussed_window = next;

    WINDOW(next)->flags |= W_HAS_FOCUS;

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

void __fastcall__
append_window (struct obj * win, char is_fullscreen)
{
    blur_windows ();
    focussed_window = append_obj (desktop, win);
    WINDOW(focussed_window)->flags |= W_HAS_FOCUS | (is_fullscreen ? W_FULLSCREEN : 0);
    if (is_fullscreen)
        set_obj_position_and_size (win, 0, 0, DESKTOP_WIDTH, DESKTOP_HEIGHT);
    layout_obj (focussed_window);
    draw_obj (focussed_window);
}

void
show_help ()
{
    blur_windows ();
    append_window (make_textview (txt_help, sizeof (txt_help), "Help", 0, DESKTOP_HEIGHT / 4, DESKTOP_WIDTH, DESKTOP_HEIGHT / 2), true);
    print_message ("Welcome to INGLE!");
}

void
send_key_event (char key)
{
    struct event * e = malloc (sizeof (struct event));
    e->type = EVT_KEYPRESS;
    e->data_char = key;

    send_event (focussed_window, e);

    free (e);
}

char
wait_for_key ()
{
    static int timer = -1;
    char key;

    while (!(key = cbm_k_getin ())) {
        send_queued_event ();

        if (++timer)
            continue;

        save_desktop_state ();
    }

    timer = -0x6000;

    return key;
}

void
desktop_loop ()
{
    char key;

    do {
        key = wait_for_key ();
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

            case '?':
                show_help ();
                continue;

            default:
                send_key_event (key);
        }
    } while (!do_shutdown);
}

void
start_desktop ()
{
    desktop = OBJ(make_box (pattern_woven));
    desktop->ops = &desktop_obj_ops;
    set_obj_position_and_size (desktop, 0, 0, DESKTOP_WIDTH, DESKTOP_HEIGHT);

    print_message ("Press '?' for help.");
    layout_obj (desktop);
    draw_obj (desktop);

    append_window (w_make_file_window (&cbm_drive_ops, "#8", 0, DESKTOP_HEIGHT / 2, DESKTOP_WIDTH, DESKTOP_HEIGHT / 2 + 1), false);

    desktop_loop ();
}
