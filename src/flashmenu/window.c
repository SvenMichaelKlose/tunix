#include <stdlib.h>

#include "libgfx.h"
#include "obj.h"
#include "window.h"
#include "error.h"

#define WINDOW_TITLE_HEIGHT     11

struct window * __fastcall__
make_window (gpos x, gpos y, gsize w, gsize h, char * title)
{
    struct window * win = alloc_obj (sizeof (struct window), x, y, w, h, draw_window);
    win->title = title;
    return win;
}

void
draw_window_content (struct window * win)
{
    if (!win->obj.node.children)
        print_error ("no kids");
    draw_obj (win->obj.node.children);
}

void __fastcall__
draw_window (void * _w)
{
    struct window * win = _w;
    struct rect * r = &win->obj.rect;
    gpos x = r->x;
    gpos y = r->y;
    gpos y2 = y + 2;
    gsize w = r->w;
    gsize h = r->h;
    gpos ix = x + 1;
    gpos iy = y + 1;
    gsize iw = r->w - 2;
    gpos xr = x + iw - 1;
    gpos yb = y + WINDOW_TITLE_HEIGHT - 1;
    gpos cx;
    gsize gw;

    /* Draw window title. */
    gfx_reset_region ();
    gfx_set_pencil_mode (PENCIL_MODE_OR);
    gfx_set_pattern (pattern_empty);
    gfx_set_font (charset_4x8, 2);
    gfx_draw_box (x, y, w, WINDOW_TITLE_HEIGHT);
    gfx_set_pattern (pattern_solid);
    gfx_draw_frame (x, y, w, WINDOW_TITLE_HEIGHT);
/*
    gfx_set_region (ix, iy, iw, WINDOW_TITLE_HEIGHT - 2);
*/
    gfx_draw_text (ix + 1, iy + 1, win->title);

    /* Draw title grip. */
    cx = gfx_x ();
    if (cx <= xr) {
        gw = xr - cx + 1;
        for (; y2 < yb; y2 += 2)
            gfx_draw_hline (cx, y2, gw);
    }

    /* Draw contents. */
    gfx_draw_frame (x, y + WINDOW_TITLE_HEIGHT - 1, w, h - WINDOW_TITLE_HEIGHT + 1);
    gfx_set_pattern (pattern_empty);
    gfx_draw_box (ix, y + WINDOW_TITLE_HEIGHT, iw, h - WINDOW_TITLE_HEIGHT - 1);

/*
    gfx_set_region (ix, y + WINDOW_TITLE_HEIGHT, iw, h - WINDOW_TITLE_HEIGHT);
*/
    draw_window_content (win);
}
