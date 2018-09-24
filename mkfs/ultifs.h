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
    usize   size;           /* Current size (grows on writes). */
    upos    directory;      /* The directory this file is in. */
    upos    replaced;       /* Position of block this one replaced. Yet unused? */
    char    mode;           /* Mode at bfile_open(). */
};
typedef struct _bfile bfile;

extern upos ultifs_pwd;

extern bfile * __cc65fastcall__ bfile_create (upos directory, char * name, char type);
extern void    __cc65fastcall__ bfile_remove (bfile *);
extern void    __cc65fastcall__ bfile_close (bfile *);
extern upos    __cc65fastcall__ bfile_create_directory (upos parent, char * name);

#ifdef __CC65__
extern struct cbm_dirent;
extern bfile * __cc65fastcall__ ultifs_open (upos directory, char * name, char mode);
extern int     __cc65fastcall__ bfile_readm (bfile * b, char * bytes, unsigned len);

extern char    __cc65fastcall__ ultifs_opendir (void);
extern char    __cc65fastcall__ ultifs_readdir (struct cbm_dirent *);
extern void                     ultifs_closedir (void);
extern char    __cc65fastcall__ ultifs_enterdir (char * name);
#endif

#endif // #ifndef ULTIFS_H
