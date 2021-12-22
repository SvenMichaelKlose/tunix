#ifndef SCREEN_H
#define SCREEN_H

extern void screen_init     (void);
extern void screen_redraw   (void);
extern void __fastcall__ screen_set_status  (char * msg);

#endif // #ifndef SCREEN_H
