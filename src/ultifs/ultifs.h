#ifndef ULTIFS_H
#define ULTIFS_H

// 8 unused bytes in Flash ROM.  Replaces dword NULL.
#define ULTIFS_UNUSED   ((upos) -1)

#ifndef __CC65__
typedef unsigned int upos;
typedef unsigned int usize;
#define __cc65fastcall__
#else
typedef unsigned long upos;
typedef unsigned long usize;
#define __cc65fastcall__    __fastcall__
#endif

enum ultifs_error_e {
    ULTIFS_ERR_OK = 0,
    ULTIFS_ERR_END_OF_FILE,
    ULTIFS_ERR_FILE_NOT_IN,
    ULTIFS_ERR_FILE_NOT_OUT
};
extern char ultifs_error;

#define ULTIFS_START    0x10000

#define ULTIFS_MODE_READ    0
#define ULTIFS_MODE_WRITE   1

typedef struct _bfile {
    upos       start;      /* Start of file block. */
    upos       ptr;        /* File data position. */
    upos       pos;        /* Position in file data. */
    usize      size;       /* Current size (grows on writes). */
    upos       directory;  /* The directory this file is in. */
    upos       replaced;   /* Position of block this one replaced. */
    char       mode;       /* Mode at bfile_open(). */
} bfile;

#define ULTIFS_ROOT_DIR     0x10000

extern upos ultifs_pwd;

extern char ultifs_mount (void);

//extern bfile * __cc65fastcall__ bfile_create (upos directory, char * name, char type);
extern void    __cc65fastcall__ bfile_remove (bfile *);
extern void    __cc65fastcall__ bfile_close (bfile *);
extern upos    __cc65fastcall__ bfile_create_directory (upos parent, char * name);

#ifdef __CC65__
extern struct cbm_dirent;
extern bfile * __cc65fastcall__ ultifs_create (upos directory, char * name, char type);
extern bfile * __cc65fastcall__ ultifs_open (upos directory, char * name, char mode);
//extern void    __cc65fastcall__ ultifs_close (bfile *);
extern int     __cc65fastcall__ bfile_readm (bfile *, char * bytes, unsigned len);
extern char    __cc65fastcall__ bfile_read (bfile *);
extern void    __cc65fastcall__ bfile_write (bfile *, char);

extern char                     ultifs_opendir (void);
extern char    __cc65fastcall__ ultifs_readdir (struct cbm_dirent *);
extern void                     ultifs_closedir (void);
extern upos    __cc65fastcall__ ultifs_enterdir (upos parent, char * name);
extern void                     ultifs_leavedir (void);
#endif

#endif // #ifndef ULTIFS_H
