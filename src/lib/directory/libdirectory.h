#ifndef __LIBDIRECTORY_H__
#define __LIBDIRECTORY_H__

struct dirent {
    char            name[17];
    unsigned long   size;
    char            type;
    struct dirent * next;
};

extern char            __fastcall__ directory_open ();
extern char            __fastcall__ directory_read (struct cbm_dirent * dirent);
extern void            __fastcall__ directory_close ();
extern struct dirent * __fastcall__ make_directory_list (unsigned *);
extern void            __fastcall__ free_directory_list (struct dirent *);

#endif // #define __LIBDIRECTORY_H__
