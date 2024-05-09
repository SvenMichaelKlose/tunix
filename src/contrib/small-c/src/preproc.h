#ifndef __PREPROC_H__
#define __PREPROC_H__

extern char keepch (char c);
extern void toggle (char name, int onoff);
extern char remove_one_line_comment (char c);
extern char putmac (char c);
extern void addmac (void);
extern void delmac (int mp);
extern int findmac (char *sname);
extern FILE * fix_include_name (void);
extern void doinclude (void);
extern void doasm (void);
extern void dodefine (void);
extern void doundef (void);
extern void doifdef (int ifdef);
extern void noiferr (void);
extern int ifline (void);
extern void defmac (char *);
extern int cpp (void);
extern void preprocess (void);

#endif // #ifndef __PREPROC_H__
