#ifndef __IO_H__
#define __IO_H__

extern int openin (char *p);
extern int openout (void);
extern void outfname (char *s);
extern void fixname (char *s);
extern int checkname (char *s);
extern void kill (void);
extern void readline (void);
extern char inbyte (void);
extern char inchar (void);
extern char gch (void);
extern char nch (void);
extern char ch (void);
extern void pl (char *str);

#endif // #ifndef __IO_H__
