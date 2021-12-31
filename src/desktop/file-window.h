#ifndef FILE_WINDOW_H
#define FILE_WINDOW_H

#include <lib/gui/window.h>

#define FILE_WINDOW_BANK    7

struct dirent {
    char            name[17];
    unsigned long   size;
    unsigned char   type;
    struct dirent * next;
};

struct drive_ops {
    char                    (*opendir)  (void);
    char __fastcall__       (*readdir)  (struct cbm_dirent *);
    void                    (*closedir) (void);
    char __fastcall__       (*enterdir) (char * name);
    void                    (*leavedir) (void);
    char __fastcall__       (*open)     (char * name, char mode);
    int  __fastcall__       (*read)     (void *, unsigned);
    void                    (*close)    (void);
    unsigned __fastcall__   (*launch)   (struct drive_ops *, unsigned start);
};

extern struct drive_ops cbm_drive_ops;
extern struct drive_ops ultifs_drive_ops;

struct file_window_content {
    struct obj          obj;
    struct drive_ops *  drive_ops;
    struct dirent *     files;
    int    len;
    int    pos;    /* User's position in list. */
};

extern struct obj * __fastcall__ make_file_window (struct drive_ops *, char * title, gpos x, gpos y, gsize w, gsize h);
extern struct obj * __fastcall__ w_make_file_window (struct drive_ops *, char * title, gpos x, gpos y, gsize w, gsize h);

#endif /* #ifndef FILE_WINDOW_H */
