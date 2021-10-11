#ifndef LINE_H
#define LINE_H

#define MAX_LINE_LENGTH     256

typedef struct _linedata linedata;

typedef struct _linedata {
    unsigned    version;
    unsigned    version_deleted;
    linedata    * newer;
} linedata;

typedef struct _line line;

typedef struct _line {
    line   * prev;
    line   * next;
    char        data;
} line;

void line_init     (void);

void line_clear         (void);
void line_redraw        (void);
void line_commit        (void);
void line_delete        (void);
void line_insert        (void);
void line_open          (void);

void line_insert_char   (char c);
void line_delete_char   (void);

void line_move_left     (void);
void line_move_right    (void);
char line_move_down     (void);

void line_test          (void);

void screen_redraw      (void);

#endif // #ifndef LINE_H
