#ifndef __LIBLISP_IO_H__
#define __LIBLISP_IO_H__

#define STDIN  0
#define STDOUT 3

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern char last_out;   // Last output char.
#ifdef __CC65__
#pragma zpsym ("last_out")
#pragma bss-name (pop)
#endif

extern char eof (void);
extern char err (void);
extern void setin (char fn);
extern void setout (char fn);

extern char in (void);
extern char ch (void);
extern void putback (void);
extern void skip_spaces (void);

extern void out (char c);
extern void out_number (int);
extern void outs (char *);
extern void outsn (char *, char len);
extern void terpri (void);

// Start output on error channel (e.g. screen).
extern void errouts (char *);

#endif // #ifndef __LIBLISP_IO_H__
