#include <stdlib.h>

#include "libgfx.h"
#include "obj.h"
#include "layout-ops.h"
#include "window.h"
#include "message.h"
#include "frame.h"

#define WINDOW_TITLE_HEIGHT     11

void __fastcall__ layout_window_content_frame (struct obj *);

struct obj_ops window_ops = {
    draw_window,
    layout_window_content_frame
};

struct window * __fastcall__
make_window (char * title)
{
    struct window * win = alloc_obj (sizeof (struct window), &window_ops);
    struct obj * f = make_frame ();
    append_obj (OBJ(win), f);
    win->title = title;
    return win;
}

void __fastcall__
draw_window (struct obj * _w)
{
    struct window * win = (struct window *) _w;
    struct rect * r = &win->obj.rect;
    gpos x = r->x;
    gpos y = r->y;
    gsize w = r->w;
    gsize h = r->h;
    gsize iw = r->w - 2;
    gpos xr = iw - 1;
    gpos yb = y + WINDOW_TITLE_HEIGHT - 1;
    gpos cx;
    gsize gw;
    gpos y2;

    /* Draw window title. */
    gfx_push_context ();
    gfx_set_region (x, y, w, WINDOW_TITLE_HEIGHT);
    gfx_set_pencil_mode (PENCIL_MODE_OR);
    gfx_set_pattern (pattern_empty);
    gfx_set_font (charset_4x8, 2);
    gfx_draw_box (0, 0, w, WINDOW_TITLE_HEIGHT);
    gfx_set_pattern (pattern_solid);
    gfx_draw_frame (0, 0, w, WINDOW_TITLE_HEIGHT);
    gfx_draw_text (2, 2, win->title);

    /* Draw title grip. */
    cx = gfx_x ();
    if (cx <= xr) {
        gw = xr - cx + 1;
        for (y2 = 2; y2 < yb; y2 += 2)
            gfx_draw_hline (cx, y2, gw);
    }
    gfx_pop_context ();

    /* Draw content frame. */
    gfx_push_context ();
    gfx_set_pattern (pattern_solid);
    gfx_set_region (x, y + WINDOW_TITLE_HEIGHT - 1, w, h - WINDOW_TITLE_HEIGHT);
    gfx_draw_frame (0, 0, w, h - WINDOW_TITLE_HEIGHT + 1);
    gfx_set_pattern (pattern_empty);
    gfx_draw_box (1, 1, iw, h - WINDOW_TITLE_HEIGHT - 1);

    /* Draw contents. */
    draw_obj_children (OBJ(win));
    gfx_pop_context ();
}

void
layout_window_content_frame (struct obj * win)
{
    set_obj_position_and_size (win->node.children,
                               1, 1,
                               win->rect.w - 2, win->rect.h - WINDOW_TITLE_HEIGHT - 1);
    layout_obj_children (win);
}
