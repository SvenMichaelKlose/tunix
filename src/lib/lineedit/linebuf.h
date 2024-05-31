#ifndef LINEBUF_H
#define LINEBUF_H

#ifdef __CC65__
#define FASTCALL __fastcall__
#else
#define FASTCALL
#endif

#define MAX_LINE_LENGTH     256

typedef unsigned pos_t;

extern char linebuf[MAX_LINE_LENGTH + 1];
extern unsigned linebuf_length;

extern void linebuf_clear (void);
extern void FASTCALL linebuf_insert_char (pos_t, char);
extern void linebuf_delete_char (pos_t);
extern void linebuf_replace_char (pos_t, char);

#endif // #ifndef LINEBUF_H
