#ifndef WINDOW_H
#define WINDOW_H

extern struct obj_ops window_ops;

struct window {
    struct obj  obj;
    char *      title;
};

struct window * __fastcall__ make_window (char * title);
void __fastcall__ draw_window (struct obj * _w);

#endif /* #ifndef WINDOW_H */
