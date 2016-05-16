#ifndef BUTTON_H
#define BUTTON_H

extern struct obj_ops button_ops;

struct button {
    struct obj  obj;
    char *      text;
};

struct button * __fastcall__ make_button (char * text);
void __fastcall__ draw_button (void *);

#endif /* #ifndef BUTTON_H */
