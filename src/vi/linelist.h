#ifndef LINELIST_H
#define LINELIST_H

/*
typedef struct _linedata linedata;

typedef struct _linedata {
    unsigned    version;
    unsigned    version_deleted;
    linedata    * newer;
} linedata;
*/

extern int linenr;
extern unsigned num_lines;

typedef struct _line line;

typedef struct _line {
    line   * prev;
    line   * next;
    char   data;
} line;

extern void    linelist_init           (void);
extern line *  linelist_get            (unsigned);
extern void    linelist_goto           (unsigned);
extern void    linelist_append         (void);
extern void    linelist_delete         (void);
extern void    linelist_insert_before  (void);
extern void    linelist_insert_after   (void);

#endif // #ifndef LINELIST_H
