#ifndef BUTTON_H
#define BUTTON_H

#include "obj.h"

struct button {
    struct obj  obj;
    char *      text;
};

extern struct button * __fastcall__ make_button (short x, short y, short w, short h, char * text);
extern void __fastcall__ draw_button (void *);

#endif /* #ifndef BUTTON_H */
