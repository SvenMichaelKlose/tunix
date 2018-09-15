#ifndef WINDOW_H
#define WINDOW_H

extern struct obj_ops window_ops;

#define W_FULLSCREEN    1

struct window {
    struct obj  obj;
    char *      title;
    char        flags;
};

struct window * __fastcall__ make_window (char * title, struct obj * content);
void __fastcall__ draw_window (struct obj * _w);

#endif /* #ifndef WINDOW_H */
