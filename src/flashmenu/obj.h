#ifndef OBJ_H
#define OBJ_H

#define OBJ(x)  ((struct obj *) x)

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

typedef void (*func_draw_t) (void *);

struct obj_ops {
    func_draw_t     draw;
};

struct obj {
    struct treenode   node;
    struct rect       rect;
    struct obj_ops *  ops;
};

struct scrollable {
    struct obj  obj;
    char        bank;
};

extern void * __fastcall__ alloc_obj (size_t size, gpos x, gpos y, gsize w, gsize h, struct obj_ops *);
extern void __fastcall__ free_obj (struct obj *);
extern void __fastcall__ draw_obj (struct obj *);
extern void __fastcall__ append_obj (struct obj * parent, struct obj * x);

#endif /* #ifndef OBJ_H */
