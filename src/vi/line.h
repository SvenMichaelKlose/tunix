#ifndef LINE_H
#define LINE_H

#define MAX_LINE_LENGTH     256

typedef struct _line line;

typedef struct _line {
    unsigned    version;
    unsigned    version_deleted;
    line        * newer;
} line;

typedef struct _linestack linestack;

typedef struct _linestack {
    linestack   * next;
    line        first;
} linestack;

void line_move_left (void);
void line_move_right (void);

#endif // #ifndef LINE_H
