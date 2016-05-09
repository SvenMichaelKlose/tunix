#ifndef WINDOW_H
#define WINDOW_H

struct window {
    struct obj  obj;
    char *      title;
    char        canvas_bank;
};

extern struct window * __fastcall__ make_window (short x, short y, short w, short h, char * title);
extern void __fastcall__ draw_window (void * _w);

#endif /* #ifndef WINDOW_H */
