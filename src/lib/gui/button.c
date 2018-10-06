#include <stdlib.h>

#include "libgfx.h"

#include "obj.h"
#include "event.h"
#include "button.h"
#include "layout-ops.h"

void __fastcall__ layout_button_minsize (struct obj *);

struct obj_ops button_ops = {
    draw_button,
    layout_button_minsize,
    obj_noop,
    event_handler_passthrough
};

struct button * __fastcall__
make_button (char * text)
{
    struct button * b = alloc_obj (sizeof (struct button), &button_ops);
    b->obj.rect.h = 12;
    b->text = text;
    return b;
}

void __fastcall__
draw_button (struct obj * _b)
{
    struct button * b = (struct button *) _b;
    struct rect * r = &b->obj.rect;
    gsize textwidth;

    gfx_set_font (charset_4x8, 2, FONT_BANK);
    textwidth = gfx_get_text_width (b->text);
    gfx_set_pencil_mode (1);
//    gfx_set_pattern (pattern_empty);
//    gfx_draw_box (r->x + 1, r->y + 1, r->w - 2, r->h - 2);
    gfx_set_pattern (pattern_solid);
    gfx_draw_frame (r->x, r->y, r->w, r->h);
    gfx_draw_text (r-> x + (r->w - textwidth) / 2 + 1, r->y + (r->h - 8) / 2 + 1, b->text);
}

void __fastcall__
layout_button_minsize (struct obj * x)
{
    struct button * b = (struct button *) x;
    gsize textwidth;

    gfx_set_font (charset_4x8, 2, FONT_BANK);
    textwidth = gfx_get_text_width (b->text);
    x->rect.w = textwidth + 4;
}
