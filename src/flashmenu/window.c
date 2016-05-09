#include <stdlib.h>

#include "libgfx.h"
#include "obj.h"
#include "window.h"

#define WINDOW_TITLE_HEIGHT     11

void
errnomem ()
{
    gfx_clear_screen (1);
    gfx_set_font (charset_4x8, 2);
    gfx_set_pencil_mode (1);
    gfx_draw_text (0, 84, "Error: Out of heap memory.");
    while (1);
}

struct window * __fastcall__
make_window (short x, short y, short w, short h, char * title)
{
    struct window * win = malloc (sizeof (struct window));
    struct obj * obj;
    struct treenode * node;
    struct rect * rect;

    if (!win)
        errnomem ();

	obj = &win->obj;
    node = &obj->node;
    node->prev = NULL;
    node->next = NULL;
    node->children = NULL;

	rect = &obj->rect;
	rect->x = x;
	rect->y = y;
	rect->w = w;
	rect->h = h;
    win->title = title;

    return win;
}

void __fastcall__
draw_window (void * _w)
{
    struct window * win = _w;
    struct rect * r = &win->obj.rect;
    short x = r->x;
    short y = r->y;
    short y2 = y + 2;
    short w = r->w;
    short h = r->h;
    short ix = x + 1;
    short iy = y + 1;
    short iw = r->w - 2;
    short xr = x + iw - 1;
    short yb = y + WINDOW_TITLE_HEIGHT - 1;
    short cx;
    short gw;

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
    draw_obj (win->obj.node.children);
}
