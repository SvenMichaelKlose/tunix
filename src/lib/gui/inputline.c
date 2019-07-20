#include <cc65-charmap.h>

#include <cbm.h>
#include <stdlib.h>

#include "libgfx.h"

#include "obj.h"
#include "event.h"
#include "inputline.h"
#include "layout-ops.h"
#include "message.h"

extern struct obj * inputline;
gpos inputline_x;
unsigned char inputline_pos;
char * inputline_buf = NULL;
char * inputline_widths = NULL;

void __fastcall__ layout_inputline_minsize (struct obj *);

struct obj_ops inputline_ops = {
    draw_inputline,
    layout_inputline_minsize,
    obj_noop,
    event_handler_passthrough
};

struct inputline * __fastcall__
make_inputline (char * text)
{
    struct inputline * b = alloc_obj (sizeof (struct inputline), &inputline_ops);
    b->obj.rect.h = 12;
    b->text = text;

    inputline_x = 0;
    inputline_pos = 0;
    if (!inputline_buf) {
        inputline_buf = malloc (256);
        inputline_widths = malloc (256);
    }

    return b;
}

void __fastcall__
draw_inputline (struct obj * _b)
{
    struct inputline * b = INPUTLINE(_b);
    struct rect * r = &b->obj.rect;
    gsize textwidth;

    gfx_set_font (charset_4x8, 2, FONT_BANK);
    textwidth = gfx_get_text_width (b->text);
    gfx_set_pencil_mode (1);
    gfx_set_pattern (pattern_empty);
    gfx_draw_box (r->x + 1, r->y + 1, r->w - 2, r->h - 2);
    gfx_set_pattern (pattern_solid);
    gfx_draw_frame (r->x, r->y, r->w, r->h);
    gfx_draw_text (r-> x + 2, r->y + (r->h - 8) / 2 + 1, b->text);
//    gfx_draw_text (r-> x + (r->w - textwidth) / 2 + 1, r->y + (r->h - 8) / 2 + 1, b->text);
}

void __fastcall__
layout_inputline_minsize (struct obj * x)
{
    struct inputline * b = INPUTLINE(x);
    gsize textwidth;

    gfx_set_font (charset_4x8, 2, FONT_BANK);
    textwidth = gfx_get_text_width (b->text);
    x->rect.w = textwidth + 4;
}

void
inputline_init_draw ()
{
    struct rect * r = &inputline->rect;
    gfx_reset_region ();
    gfx_set_region (r->x, r->y, r->w, r->h);
}

void __fastcall__
inputline_insert (char c)
{
    gpos old_x = gfx_x ();
    unsigned char char_width;

    inputline_init_draw ();
    gfx_set_position (inputline_x + 2, 2);
    gfx_putchar (c);
    inputline_bug[inputline_pos] = c;
    char_width = gfx_x () - old_x;
    inputline_x += char_width;
    inputline_widths[inputline_pos] = char_width;
    ++inputline_pos;
}

void __fastcall__
inputline_input (char c)
{
    inputline_insert (c);
}
