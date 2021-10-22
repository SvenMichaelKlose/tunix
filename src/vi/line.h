#ifndef LINE_H
#define LINE_H

#define MAX_LINE_LENGTH     256

/*
typedef struct _linedata linedata;

typedef struct _linedata {
    unsigned    version;
    unsigned    version_deleted;
    linedata    * newer;
} linedata;
*/

typedef struct _line line;

typedef struct _line {
    line   * prev;
    line   * next;
    char        data;
} line;

extern void line_init       (void);
extern void line_commit     (void);
extern void line_delete     (void);
extern void line_insert     (void);
extern void line_open_below (void);
extern char line_move_down  (void);
extern void screen_redraw   (void);

#endif // #ifndef LINE_H
