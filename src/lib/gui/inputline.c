#include <cc65-charmap.h>

#include <cbm.h>
#include <stdlib.h>
#include <string.h>

#include "libgfx.h"

#include "obj.h"
#include "event.h"
#include "inputline.h"
#include "layout-ops.h"
#include "message.h"

#define MAX_INPUTLINE_LENGTH    256

#define KEY_RUNSTOP     3
#define KEY_DELETE      20
#define KEY_LEFT        157
#define KEY_RIGHT       29

extern struct obj * inputline;
gpos inputline_x;
unsigned char inputline_pos;
char * inputline_buf = NULL;
char * inputline_widths = NULL;

void __fastcall__ layout_inputline_minsize (struct obj *);

struct obj_ops inputline_ops = {
    draw_inputline,
    obj_noop,
    obj_noop,
    event_handler_passthrough
};

void
inputline_init_draw ()
{
    gfx_reset_region ();
    set_obj_region (inputline);
    gfx_set_font (charset_4x8, 2, FONT_BANK);
    gfx_set_pencil_mode (1);
    gfx_set_pattern (pattern_empty);
}

void
inputline_show_cursor (void)
{
    gfx_set_pattern (pattern_solid);
    gfx_draw_vline (inputline_x, 0, 8);
}

void
inputline_hide_cursor (void)
{
    gfx_set_pattern (pattern_empty);
    gfx_draw_vline (inputline_x, 0, 8);
}

void __fastcall__
inputline_insert (char c)
{
    gpos old_x = gfx_x ();
    unsigned char char_width;

    if (inputline_pos == MAX_INPUTLINE_LENGTH - 2)
        return;

    inputline_init_draw ();
    inputline_hide_cursor ();
    gfx_set_position (inputline_x, 0);
    gfx_putchar (c);
    inputline_buf[inputline_pos] = c;
    inputline_buf[inputline_pos + 1] = 0;
    char_width = gfx_x () - old_x;
    inputline_x += char_width;
    inputline_widths[inputline_pos] = char_width;
    ++inputline_pos;
    inputline_show_cursor ();
}

void
inputline_delete (void)
{
    gpos old_x = gfx_x ();
    unsigned char char_width = inputline_widths[inputline_pos - 1];

    if (!inputline_pos)
        return;

    inputline_hide_cursor ();
    inputline_init_draw ();
    gfx_set_pattern (pattern_empty);
    inputline_x -= char_width;
    gfx_draw_box (inputline_x, 0, char_width, 8);
    inputline_buf[inputline_pos] = 0;
    --inputline_pos;
    inputline_show_cursor ();
}

void __fastcall__
inputline_input (char c)
{
    if (c == KEY_DELETE)
        inputline_delete ();
    else
        inputline_insert (c);
}

void __fastcall__
draw_inputline (struct obj * _b)
{
    struct inputline * b = INPUTLINE(_b);
    struct rect * r = &b->obj.rect;

    inputline_init_draw ();
    gfx_draw_box (r->x, r->y, r->w, r->h);
    gfx_draw_text (0, 0, inputline_buf);

    inputline_show_cursor ();
}

struct inputline * __fastcall__
make_inputline (char * text)
{
    struct inputline * b = alloc_obj (sizeof (struct inputline), &inputline_ops);
    char c;

    b->obj.rect.h = 8;

    inputline_x = 0;
    inputline_pos = 0;
    if (!inputline_buf) {
        inputline_buf = malloc (MAX_INPUTLINE_LENGTH);
        inputline_widths = malloc (MAX_INPUTLINE_LENGTH);
    }

    while (c = *text++)
        inputline_insert (c);

    return b;
}

void
inputline_close ()
{
    if (!inputline)
        return;
    free (inputline_buf);
    free (inputline_widths);
    unlink_obj (inputline);
    free_obj (inputline);
    inputline = NULL;
}
