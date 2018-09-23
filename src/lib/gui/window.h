#ifndef WINDOW_H
#define WINDOW_H

extern struct obj_ops window_ops;

#define W_FULLSCREEN    1

struct window {
    struct obj  obj;
    char *      title;
    char        flags;
};

extern struct window * __fastcall__ make_window (char * title, struct obj * content, event_handler_t handler);
extern void __fastcall__ draw_window (struct obj * _w);

#endif /* #ifndef WINDOW_H */
