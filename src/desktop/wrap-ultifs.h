#ifndef WRAP_ULTIFS_H
#define WRAP_ULTIFS_H

extern void w_ultifs_mount (void);

extern bfile * __fastcall__ w_ultifs_open (upos directory, char * name, char mode);
extern int     __fastcall__ w_bfile_readm (bfile * b, char * bytes, unsigned len);
extern void    __fastcall__ w_bfile_close (bfile * b);

extern char    __fastcall__ w_ultifs_opendir (void);
extern char    __fastcall__ w_ultifs_readdir (struct cbm_dirent *);
extern void                 w_ultifs_closedir (void);
extern char    __fastcall__ w_ultifs_enterdir (char * name);
extern void                 w_ultifs_leavedir (void);

#endif // #ifndef WRAP_ULTIFS_H
