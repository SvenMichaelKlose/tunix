#ifndef __LIBLISP_IO_H__
#define __LIBLISP_IO_H__

extern char last_out;

extern char eof (void);
extern char in (void);
extern char ch (void);
extern void putback (void);
extern void skip_spaces (void);

extern void out (char);
extern void out_number (int);
extern void outs (char *);
extern void outsn (char *, char len);

// Start output on error channel (e.g. screen).
extern void errouts (char *);

#endif // #ifndef __LIBLISP_IO_H__
