#ifndef WINDOW_H
#define WINDOW_H

extern struct obj_ops window_ops;

#define W_FULLSCREEN    1

struct window {
    struct obj  obj;
    char *      title;
    char        flags;
    gpos        user_x;
    gpos        user_y;
    gsize       user_w;
    gsize       user_h;
};

extern struct window * __fastcall__ make_window (char * title, struct obj * content, event_handler_t handler);
extern void __fastcall__ draw_window (struct obj * _w);
extern void __fastcall__ window_draw_title (struct window * w);
extern void __fastcall__ window_set_position_and_size (struct window * win, gpos x, gpos y, gsize w, gsize h);

#endif /* #ifndef WINDOW_H */
