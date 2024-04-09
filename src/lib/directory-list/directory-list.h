#ifndef __DIRECTORY_LIST_H__
#define __DIRECTORY_LIST_H__

struct dirent {
    char            name[17];
    unsigned long   size;
    char            type;
    struct dirent * next;
};

extern struct dirent * __fastcall__ make_directory_list ();
extern void            __fastcall__ free_directory_list (struct dirent *);

#endif // #define __DIRECTORY_LIST_H__
