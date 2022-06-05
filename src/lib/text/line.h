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
    line      * prev;
    line      * next;
    char      * data;
    unsigned  length;
} line;

extern line * first_line;

extern void                 line_test           (char *);

extern line *               line_alloc          (void); // TODO: Shouldn't be public.
extern void                 line_init           (void);
extern line *               line_get            (unsigned);
extern void                 buf_to_line         (void);
extern void                 line_to_buf    (void);
extern void                 line_delete         (void);
extern void                 line_insert_before  (void);
extern void                 line_insert_after   (void);
extern void                 line_split          (void);
extern void                 line_join           (void);
extern void                 line_clear          (void);

#endif // #ifndef LINELIST_H
