#ifndef LIBLINEEDIT_H
#define LIBLINEEDIT_H

#define MAX_LINE_LENGTH     256

extern unsigned  xpos;
extern char      linebuf[MAX_LINE_LENGTH + 1];
extern unsigned  linebuf_length;

extern void lineedit       (char key);
extern void lineedit_init  (void);

// To be provided by user.
extern void linebuf_redraw (void);

#endif // #ifndef LIBLINEEDIT_H
