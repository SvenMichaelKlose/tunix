#ifndef OBJ_H
#define OBJ_H

#define OBJ(x)  ((struct obj *) x)

typedef void (*func_draw_t) (void *);

struct _configuration {
    char type;
};

struct rect {
    gpos x;
    gpos y;
    gsize w;
    gsize h;
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

    func_draw_t     draw;
};

struct scrollable {
    struct obj  obj;
    char        bank;
};

extern void * __fastcall__ alloc_obj (size_t size, gpos x, gpos y, gsize w, gsize h, func_draw_t);
extern void __fastcall__ free_obj (struct obj *);
extern void __fastcall__ draw_obj (struct obj *);
extern void __fastcall__ append_obj (struct obj * parent, struct obj * x);

#endif /* #ifndef OBJ_H */
