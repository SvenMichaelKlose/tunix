#include <stdlib.h>

typedef unsigned short ushort;
typedef unsigned char uchar;

void gfx_init () {}
short gfx_x () {}
void __fastcall__ gfx_clear_screen (char colour) {}
void gfx_reset_region () {}
void __fastcall__ gfx_set_region (short x, short y, short w, short h) {}
void __fastcall__ gfx_draw_hline (short x, short y, short w, char colour);
void __fastcall__ gfx_draw_vline (short x, short y, short h, char colour) {}
void __fastcall__ gfx_draw_frame (short x, short y, short w, short h, char colour) {}
void __fastcall__ gfx_draw_box (short x, short y, short w, short h, char colour) {}
void __fastcall__ gfx_draw_text (short x, short y, char * txt, char colour) {}

#define WINDOW_TITLE_HEIGHT     10

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
make_window (char * title)
{
    struct window * x = malloc (sizeof (struct window));
    if (!x)
        errnomem ();
    x->title = title;
    return x;
}

void __fastcall__
draw_window (void * _w)
{
    struct window * win = _w;
    struct rect * r = &win->obj.rect;
    short x = r->x;
    short y = r->y;
    short w = r->w;
    short h = r->h;
    short ix = x + 1;
    short iy = y + 1;
    short iw = r->w - 2;
    short xr = x + iw;
    short yb = y + WINDOW_TITLE_HEIGHT - 1;
    short cx;
    short gw;

    /* Draw window title. */
    gfx_reset_region ();
    gfx_draw_box (x, y, w, WINDOW_TITLE_HEIGHT, 0);
    gfx_draw_frame (x, y, w, WINDOW_TITLE_HEIGHT, 1);
    gfx_set_region (ix, iy, iw, WINDOW_TITLE_HEIGHT - 2);
    gfx_draw_text (ix, iy, win->title, 1);

    /* Draw title grip. */
    cx = gfx_x ();
    if (cx <= xr) {
        gw = xr - cx;
        for (y = y + 2; y < yb; y += 2)
            gfx_draw_hline (x, y, gw, 1);
    }

    /* Draw contents. */
    gfx_set_region (ix, y + WINDOW_TITLE_HEIGHT, iw, h - WINDOW_TITLE_HEIGHT);
    draw_obj (win->obj.node.children);
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
    gfx_init ();
	while (1);
    gfx_clear_screen (WHITE);
    gfx_reset_region ();

    probe_devices ();
    seek_and_load_config ();

    return 0;
}
