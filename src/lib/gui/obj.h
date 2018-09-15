#ifndef OBJ_H
#define OBJ_H

#include "libgfx.h"
#include "event.h"

#define FALSE   0
#define TRUE    1

typedef unsigned char uchar;

struct _configuration {
    char type;
};

// Position and size of an object.
struct rect {
    gpos x;
    gpos y;
    gsize w;
    gsize h;
};

// Node of tree of doubly–linked lists.
struct node {
    char            type;       // Yet unused.
    struct obj *    prev;
    struct obj *    next;
    struct obj *    parent;
    struct obj *    children;   // Head node of list.
};

typedef void __fastcall__ (*func_draw_t) (struct obj *);
typedef void __fastcall__ (*func_layout_t) (struct obj *);
typedef void __fastcall__ (*func_free_t) (struct obj *);

struct obj_ops {
    func_draw_t         draw;           // Draws object.
    func_layout_t       layout;         // Layouts object before drawing.
    func_free_t         free;           // Removes object resources.
    event_handler_t     event_handler;  // Removes object resources.
};

void __fastcall__ obj_noop (struct obj *);

// The object. This must be the first element in derived objects.
struct obj {
    struct node         node;
    struct rect         rect;
    struct obj_ops *    ops;
    char                event_handler;  // In event.c/event_handlers[].
};

// Derived objects need to be cast. Therefore this convenience macro.
#define OBJ(x)  ((struct obj *) x)

void * __fastcall__ alloc_obj (size_t size, struct obj_ops *);
void __fastcall__ free_obj (struct obj *);
void __fastcall__ set_obj_size (struct obj *, gsize w, gsize h);
void __fastcall__ set_obj_position (struct obj *, gpos x, gpos y);
void __fastcall__ set_obj_position_and_size (struct obj *, gpos x, gpos y, gsize w, gsize h);

// Append new object to list of children.
void __fastcall__ append_obj (struct obj * parent, struct obj * x);

// Copy obj_ops structure most probably to replace some of its functions and
// reassign the new structure with set_obj_ops().
void __fastcall__ copy_obj_ops (struct obj_ops * dest, struct obj_ops * src);

// Assign new obj_ops structure to object.
void __fastcall__ set_obj_ops (struct obj *, struct obj_ops *);

void __fastcall__ draw_obj (struct obj *);
void __fastcall__ draw_obj_children (struct obj *);

void __fastcall__ layout_obj (struct obj *);
void __fastcall__ layout_obj_children (struct obj *);

// Set libgfx clipping and offset region to area of object.
void __fastcall__ set_obj_region (struct obj *);

#endif // #ifndef OBJ_H
