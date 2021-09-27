#ifndef LINE_H
#define LINE_H

#define MAX_LINE_LENGTH     256

typedef unsigned pos_t;

void line_clear (void);
void line_redraw_until_end (void);

void line_move_left (void);
void line_move_right (void);
void __fastcall__ line_insert_char (char c);
void line_delete_char (void);

#endif // #ifndef LINE_H
