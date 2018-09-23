#ifndef ULTIFS_H
#define ULTIFS_H

#ifndef __CC65__
typedef unsigned int upos;
typedef unsigned int usize;
#define __cc65fastcall__
#else
typedef unsigned long upos;
typedef unsigned long usize;
#define __cc65fastcall__    __fastcall__
#endif

struct _bfile {
    upos    start;          /* Start of file data. */
    upos    ptr;            /* Current position in file data. */
    upos    directory;      /* The directory this file is in. */
    upos    replaced;       /* Position of block this one replaced. Yet unused? */
};
typedef struct _bfile bfile;

extern bfile * __cc65fastcall__ bfile_open (upos directory, upos p);
extern bfile * __cc65fastcall__ bfile_create (upos directory, char * name, usize size, char type);
extern void    __cc65fastcall__ bfile_remove (bfile *);
extern void    __cc65fastcall__ bfile_close (bfile *);
extern upos    __cc65fastcall__ bfile_create_directory (upos parent, char * name);

#endif // #ifndef ULTIFS_H
