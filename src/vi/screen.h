#ifndef SCREEN_H
#define SCREEN_H

extern unsigned char ypos;
extern int changes_first;
extern int changes_last;
extern char columns;
extern char rows;

extern void              gotoxy (char x, char y);
extern void              screen_init     (void);
extern void              screen_redraw   (void);
extern void              screen_update   (void);
extern void __fastcall__ screen_set_status  (char * msg);

#endif // #ifndef SCREEN_H
