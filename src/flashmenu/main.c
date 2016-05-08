#include <stdlib.h>

#include "libgfx.h"

typedef unsigned short ushort;
typedef unsigned char uchar;

#define WINDOW_TITLE_HEIGHT     11

#define WHITE   1

#define CONFIG_TYPE     1

void
errnomem ()
{
}

struct _configuration {
    char type;
};

struct rect {
    short x;
    short y;
    short w;
    short h;
};

struct treenode {
    char            type;
    struct obj *    prev;
    struct obj *    next;
    struct obj *    parent;
    struct obj *    children;
};

struct obj {
    struct treenode node;
    struct rect     rect;

    int (*draw) (void *);
};

void __fastcall__
free_obj (struct obj * x)
{
    struct obj * c;

    while (x) {
        c = x->node.children;
        if (c)
            free_obj (c);
        free (x);
        x = x->node.next;
    }
}

void __fastcall__
draw_obj (struct obj * x)
{
    while (x) {
        x->draw (x);
        x = x->node.next;
    }
}

struct scrollable {
    struct obj  obj;
    char        bank;
};

struct window {
    struct obj  obj;
    char *      title;
    char        canvas_bank;
};

struct window * __fastcall__
make_window (short x, short y, short w, short h, char * title)
{
    struct window * win = malloc (sizeof (struct window));
    struct rect * rect;

    if (!win)
        errnomem ();

	rect = &win->obj.rect;
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
    char empty_pattern[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
    char solid_pattern[8] = { 255, 255, 255, 255, 255, 255, 255, 255 };
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
    gfx_set_pattern (empty_pattern);
    gfx_draw_box (x, y, w, WINDOW_TITLE_HEIGHT);
    gfx_set_pattern (solid_pattern);
    gfx_draw_frame (x, y, w, WINDOW_TITLE_HEIGHT);
/*
    gfx_set_region (ix, iy, iw, WINDOW_TITLE_HEIGHT - 2);
*/
    gfx_draw_text (ix + 1, iy + 1, win->title);

    /* Draw title grip. */
    cx = gfx_x ();
    if (cx <= xr) {
        gw = xr - cx;
        for (; y2 < yb; y2 += 2)
            gfx_draw_hline (cx, y2, gw);
    }

    /* Draw contents. */
    gfx_draw_frame (x, y + WINDOW_TITLE_HEIGHT - 1, w, h - WINDOW_TITLE_HEIGHT);
/*
    gfx_set_region (ix, y + WINDOW_TITLE_HEIGHT, iw, h - WINDOW_TITLE_HEIGHT);
    draw_obj (win->obj.node.children);
*/
}

void
draw_scrollable (struct scrollable * x)
{}

void
draw_message (char * txt)
{}

void probe_devices ()
{
    draw_message ("Probing devices...");
}

void seek_and_load_config ()
{
    draw_message ("Seeking config...");
}

short devices;

void
init_filewindow (struct window * w, char device)
{
    /* Make window. */
    /* Draw window. */
    /* Load directory listing. */
    /* Draw window content. */
}

int
main (int argc, char ** argv)
{
	struct window * wleft;
	struct window * wright;

	char pattern[8] = {
		0xaa, 0x55,
		0xaa, 0x55,
		0xaa, 0x55,
		0xaa, 0x55
	};

    gfx_init ();
    gfx_clear_screen (WHITE);
    gfx_reset_region ();
    gfx_set_pattern (pattern);
    gfx_draw_box (0, 0, 20 * 8, 12 * 16);
	wleft = make_window (0, 0, 81, 176, "Left");
	draw_window (wleft);
	wright = make_window (80, 0, 80, 176, "Right");
	draw_window (wright);

	while (1);
    probe_devices ();
    seek_and_load_config ();

    return 0;
}
