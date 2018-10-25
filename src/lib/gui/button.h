#ifndef BUTTON_H
#define BUTTON_H

#define BUTTON(x)   ((struct button *) x)

extern struct obj_ops button_ops;

struct button {
    struct obj  obj;
    char *      text;
};

extern struct button * __fastcall__ make_button (char * text);
extern void __fastcall__ draw_button (struct obj *);

#endif /* #ifndef BUTTON_H */
