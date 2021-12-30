#ifndef LINEBUF_H
#define LINEBUF_H

#define MAX_LINE_LENGTH     256

typedef unsigned pos_t;

extern char linebuf[MAX_LINE_LENGTH + 1];
extern unsigned linebuf_length;

extern void linebuf_clear (void);
extern void __fastcall__ linebuf_insert_char (pos_t, char);
extern void linebuf_delete_char (pos_t);

#endif // #ifndef LINEBUF_H
