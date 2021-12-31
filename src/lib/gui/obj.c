#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <lib/ultimem/ultimem.h>
#include <lib/gfx/libgfx.h>
#include <lib/gui/error.h>
#include <lib/gui/obj.h>

void *
alloc_obj (size_t size, struct obj_ops * ops)
{
    struct obj * obj = calloc (1, size);
    struct node * node;
    struct rect * rect;

    if (!obj)
        error_out_of_heap_memory ();

    node = &obj->node;
    node->prev = NULL;
    node->next = NULL;
    node->parent = NULL;
    node->children = NULL;

    rect = &obj->rect;
    rect->x = 0;
    rect->y = 0;
    rect->w = 0;
    rect->h = 0;

    obj->ops = ops;

    return obj;
}

void __fastcall__
unlink_obj (struct obj * x)
{
    struct obj * n = x->node.next;
    struct obj * p = x->node.prev;

    if (n)
        n->node.prev = p;
    if (p)
        p->node.prev = n;
    if (x->node.parent == x)
        x->node.parent = n;

    x->node.next = x->node.prev = NULL;
}

void __fastcall__
free_obj (struct obj * x)
{
    struct obj * c = x->node.children;
    unsigned short oldbank = *ULTIMEM_BLK1;

    *ULTIMEM_BLK1 = x->ops->free_bank;
    while (c) {
        if (c)
            free_obj (c);
        c = c->node.next;
    }

    x->ops->free (x);
    *ULTIMEM_BLK1 = oldbank;
    free (x);
}

struct obj * __fastcall__
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

    return x;
}

void __fastcall__
set_obj_size (struct obj * o, gsize w, gsize h)
{
    struct rect * r = &o->rect;

    r->w = w;
    r->h = h;
}

void __fastcall__
set_obj_position (struct obj * o, gpos x, gpos y)
{
    struct rect * r = &o->rect;

    r->x = x;
    r->y = y;
}

void __fastcall__
set_obj_position_and_size (struct obj * o, gpos x, gpos y, gsize w, gsize h)
{
    struct rect * r = &o->rect;

    r->x = x;
    r->y = y;
    r->w = w;
    r->h = h;
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
    unsigned short oldbank = *ULTIMEM_BLK1;

    if (x->node.flags & OBJ_NODE_INVISIBLE)
        return;

    *ULTIMEM_BLK1 = x->ops->draw_bank;
    x->ops->draw (x);
    *ULTIMEM_BLK1 = oldbank;
}

void __fastcall__
draw_obj_children (struct obj * x)
{
    x = x->node.children;
    while (x) {
        draw_obj (x);
        x = x->node.next;
    }
}

void __fastcall__
layout_obj (struct obj * x)
{
    unsigned short oldbank = *ULTIMEM_BLK1;

    *ULTIMEM_BLK1 = x->ops->layout_bank;
    x->ops->layout (x);
    *ULTIMEM_BLK1 = oldbank;
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

void __fastcall__
set_obj_region (struct obj * o)
{
    gpos x = 0;
    gpos y = 0;
    gpos w = o->rect.w;
    gpos h = o->rect.h;

    do {
        x += o->rect.x;
        y += o->rect.y;
        o = o->node.parent;
    } while (o);

    gfx_set_region (x, y, w, h);
}

void __fastcall__
obj_noop (struct obj * x)
{
    (void) x;
}
