#ifndef __LIBLISP_IO_H__
#define __LIBLISP_IO_H__

extern void error (char *);
extern char eof (void);
extern char in (void);
extern char ch (void);
extern void putback (void);
extern void skip_spaces (void);
extern void out (char);
extern void out_number (int);
extern void outs (char *);
extern void outsn (char *, char len);

#endif // #ifndef __LIBLISP_IO_H__
