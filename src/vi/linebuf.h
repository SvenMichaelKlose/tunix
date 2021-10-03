#ifndef LINEBUF_H
#define LINEBUF_H

#define MAX_LINEBUF_LENGTH     256

typedef unsigned pos_t;

extern char linebuf[MAX_LINEBUF_LENGTH];
extern unsigned linebuf_length;

void linebuf_clear (void);
void __fastcall__ linebuf_insert_char (pos_t, char);
void linebuf_delete_char (pos_t);

#endif // #ifndef LINEBUF_H
