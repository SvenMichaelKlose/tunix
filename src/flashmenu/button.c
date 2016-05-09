#include <stdlib.h>

#include "libgfx.h"
#include "obj.h"
#include "button.h"

struct button * __fastcall__
make_button (short x, short y, short w, short h, char * text)
{
    struct button * b = malloc (sizeof (struct button));
    struct obj * obj;
    struct treenode * node;
    struct rect * rect;

/*
    if (!win)
        errnomem ();
*/

	obj = &b->obj;
    node = &obj->node;
    node->prev = NULL;
    node->next = NULL;
    node->children = NULL;

	rect = &obj->rect;
	rect->x = x;
	rect->y = y;
	rect->w = w;
	rect->h = h;
    b->text = text;

    return b;
}

void __fastcall__
draw_button (void * _b)
{
    struct button * b = _b;
    struct rect * r = &b->obj.rect;
    short textwidth = gfx_get_text_width (b->text);

    gfx_set_pencil_mode (1);
    gfx_set_pattern (pattern_empty);
    gfx_draw_box (r->x + 1, r->y + 1, r->w - 2, r->h - 2);
    gfx_set_pattern (pattern_solid);
    gfx_draw_frame (r->x, r->y, r->w, r->h);
    gfx_draw_text (r-> x + (r->w - textwidth) / 2, r->y + (r->h - 8) / 2, b->text);
}
