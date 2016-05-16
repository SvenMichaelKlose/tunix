#include <stdlib.h>

#include "libgfx.h"
#include "obj.h"
#include "layout-ops.h"
#include "window.h"
#include "error.h"

#define WINDOW_TITLE_HEIGHT     11

struct obj_ops window_ops = {
    draw_window,
    layout_none
};

struct window * __fastcall__
make_window (char * title)
{
    struct window * win = alloc_obj (sizeof (struct window), &window_ops);
    win->title = title;
    return win;
}

void __fastcall__
draw_window_content (struct window * win)
{
    if (!win->obj.node.children)
        print_error ("no kids");
    draw_obj_children (OBJ(win));
}

void __fastcall__
draw_window (void * _w)
{
    struct window * win = _w;
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
    gfx_push_region ();
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
    gfx_pop_region ();

    /* Draw contents. */
    gfx_set_region (x, y + WINDOW_TITLE_HEIGHT - 1, w, h - WINDOW_TITLE_HEIGHT);
    gfx_draw_frame (0, 0, w, h - WINDOW_TITLE_HEIGHT + 1);
    gfx_set_pattern (pattern_empty);
    gfx_draw_box (1, 1, iw, h - WINDOW_TITLE_HEIGHT - 1);

    gfx_push_region ();
    gfx_set_region (x + 1, y + WINDOW_TITLE_HEIGHT, iw, h - WINDOW_TITLE_HEIGHT - 1);
    draw_window_content (win);
    gfx_pop_region ();
}
