#ifndef WINDOW_H
#define WINDOW_H

extern struct obj_ops window_ops;

struct window {
    struct obj  obj;
    char *      title;
};

extern struct window * __fastcall__ make_window (gpos x, gpos y, gsize w, gsize h, char * title);
extern void __fastcall__ draw_window (void * _w);

#endif /* #ifndef WINDOW_H */
