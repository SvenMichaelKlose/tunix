#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "bank-allocator.h"
#include "obj.h"
#include "button.h"
#include "layout-ops.h"
#include "list.h"
#include "table.h"
#include "window.h"
#include "hexdump.h"
#include "cbm.h"
#include "main.h"

#define KEY_UP      145
#define KEY_DOWN    17

void
hexdump_read_directory (struct hexdump_content * content, char * path)
{
    char * buf = malloc (1024);
    unsigned i = 0;

    cbm_opendir (path, 8);
    while (!cbm_readst ()) {
        buf[i++] = cbm_read_char ();
    }
    cbm_closedir ();

    content->data = buf;
    content->len = i;
}

void __fastcall__
print_hexnibble (char v)
{
    v &= 15;
    if (v > 9)
        v = v - 10 + 'a';
    else
        v += '0';
    gfx_putchar (v);
}

void __fastcall__
print_hexbyte (char v)
{
    print_hexnibble (v >> 4);
    print_hexnibble (v);
}

void __fastcall__
print_hexword (unsigned v)
{
    print_hexbyte (v >> 8);
    print_hexbyte (v);
}

void __fastcall__
hexdump_draw_list (struct obj * w)
{
    struct window * win = (struct window *) w;
    struct hexdump_content * content = (struct hexdump_content *) w;
    char * data = content->data;
    unsigned y = 0;
    unsigned pos = content->pos;
    char xofs;
    uchar i;

    gfx_push_context ();
    while (1) {
        xofs = 1;
        if (y > w->rect.h)
            break;

        /* Clear entry. */
        gfx_set_pattern (pattern_empty);
        gfx_draw_box (0, y, w->rect.w, 8);

        if (pos >= content->len)
            goto next;

        /* Print address. */
        gfx_set_font (charset_4x8, 0);
        gfx_set_font_compression (1);
        gfx_set_position (xofs, y);
        print_hexword (pos);
        xofs += 16;

        for (i = 0; i < 8; i++) {
            if (pos >= content->len)
                break;
            xofs += 4;
            gfx_set_position (xofs, y);
            print_hexbyte (content->data[pos++]);
            xofs += 8;
        }

        pos -= 8;
        xofs += 4;
        for (i = 0; i < 8; i++) {
            if (pos >= content->len)
                break;
            gfx_set_position (xofs, y);
            gfx_putchar (content->data[pos++]);
            xofs += 4;
        }

next:
        y += 8;
    }
    gfx_pop_context ();
}


void __fastcall__
hexdump_draw (struct obj * w)
{
    struct hexdump_content * content = (struct hexdump_content *) w;

    gfx_push_context ();
    gfx_reset_region ();
    set_obj_region (w);
    hexdump_draw_list (w);
    gfx_pop_context ();
}

char
hexdump_event_handler (struct obj * o, struct event * e)
{
    struct hexdump_content * content = (struct hexdump_content *) o->node.children;
    int visible_bytes = (content->obj.rect.h / 8) * 8;

    switch (e->data_char) {
        case KEY_UP:
            if (content->len != -1 && !content->pos)
                goto done;
            content->pos -= visible_bytes;
            break;

        case KEY_DOWN:
            if (content->len != -1 && (content->pos + visible_bytes) >= content->len)
                goto done;
            content->pos += visible_bytes;
            break;
    }

    hexdump_draw ((struct obj *) content);

done:
    return FALSE;
}

struct obj_ops obj_ops_hexdump_content = {
    hexdump_draw,
    obj_noop,
    obj_noop,
    event_handler_passthrough
};

struct hexdump_content *
make_hexdump_content ()
{
	struct obj * obj =  alloc_obj (sizeof (struct obj), &obj_ops_hexdump_content);
    struct hexdump_content * content = malloc (sizeof (struct hexdump_content));

    memcpy (content, obj, sizeof (struct obj));
    free (obj);

    return content;
}

struct obj * __fastcall__
make_hexdump (char * data, unsigned len, char * title, gpos x, gpos y, gpos w, gpos h)
{
    struct hexdump_content * content = make_hexdump_content ();
	struct window * win = make_window (title, (struct obj *) content, hexdump_event_handler);

    content->data = data;
    content->len = len;
    win->flags |= W_FULLSCREEN;
	set_obj_position_and_size (OBJ(win), x, y, w, h);

    return OBJ(win);
}
