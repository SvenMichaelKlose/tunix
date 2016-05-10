#include <stdlib.h>
#include <stdio.h>

#include "libgfx.h"
#include "error.h"
#include "obj.h"

void *
alloc_obj (size_t size, gpos x, gpos y, gsize w, gsize h, func_draw_t draw)
{
    struct obj * obj = malloc (size);
    struct treenode * node;
    struct rect * rect;

    if (!obj)
        error_out_of_heap_memory ();

    node = &obj->node;
    node->prev = NULL;
    node->next = NULL;
    node->parent = NULL;
    node->children = NULL;

    rect = &obj->rect;
    rect->x = x;
    rect->y = y;
    rect->w = w;
    rect->h = h;

    obj->draw = draw;

    return obj;
}

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

void __fastcall__
append_obj (struct obj * parent, struct obj * x)
{
    struct obj * c = parent->node.children;
    struct obj * n;

    if (!c) {
        parent->node.children = x;
        return;
    }
    while (n = c->node.next)
        c = n;
    c->node.next = x;
}
