#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libgfx.h"
#include "error.h"
#include "obj.h"

void *
alloc_obj (size_t size, gpos x, gpos y, gsize w, gsize h, struct obj_ops * ops)
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

    obj->ops = ops;

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
append_obj (struct obj * parent, struct obj * x)
{
    struct obj * c = parent->node.children;
    struct obj * n;

    if (!c) {
        parent->node.children = x;
        goto link_parent;
    }
    while (n = c->node.next)
        c = n;
    c->node.next = x;
    x->node.prev = c;

link_parent:
    x->node.parent = parent;
}

void __fastcall__
copy_obj_ops (struct obj_ops * dest, struct obj_ops * src)
{
    memcpy (dest, src, sizeof (struct obj_ops));
}

void __fastcall__
set_obj_ops (struct obj * x, struct obj_ops * o)
{
    x->ops = o;
}

void __fastcall__
draw_obj (struct obj * x)
{
    x->ops->draw (x);
}

void __fastcall__
draw_obj_children (struct obj * x)
{
    x = x->node.children;
    while (x) {
        x->ops->draw (x);
        x = x->node.next;
    }
}

void __fastcall__
layout_obj (struct obj * x)
{
    x->ops->layout (x);
}

void __fastcall__
layout_obj_children (struct obj * x)
{
    x = x->node.children;
    while (x) {
        x->ops->layout (x);
        x = x->node.next;
    }
}
