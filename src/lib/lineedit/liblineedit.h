#ifndef LIBLINEEDIT_H
#define LIBLINEEDIT_H

#define MAX_LINE_LENGTH     256

typedef unsigned pos_t;

#define FALSE   0
#define TRUE    1

extern pos_t xpos;
extern pos_t ypos;

extern char linebuf[MAX_LINE_LENGTH + 1];
extern unsigned linebuf_length;

extern void set_cursor         (void);
extern void enable_cursor      (void);
extern void disable_cursor     (void);

extern void line_clear         (void);
extern void line_insert_char   (char c);
extern void line_delete_char   (void);
extern void line_move_left     (void);
extern void line_move_right    (void);
extern void line_edit          (char key);

// To be provided externally.
extern void line_redraw        (void);

#endif // #ifndef LIBLINEEDIT_H
