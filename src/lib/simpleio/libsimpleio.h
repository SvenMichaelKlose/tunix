#ifndef __LIBLISP_IO_H__
#define __LIBLISP_IO_H__

#define STDIN  0
#define STDOUT 3

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern char fnin;     // Input file number.
extern char fnout;    // Output file number.
extern char last_in;  // Last input char.
extern char last_out; // Last output char.
#ifdef __CC65__
#pragma zpsym ("fnin")
#pragma zpsym ("fnout")
#pragma zpsym ("last_in")
#pragma zpsym ("last_out")
#pragma bss-name (pop)
#endif

extern char eof (void);
extern char err (void);
extern void setin (char fn);
extern void setout (char fn);

extern char in (void);      // Read char.
extern char ch (void);      // Last read char.
extern void putback (void); // Put back char for in().
extern void skip_spaces (void);

extern void out (char c);
extern void out_number (int);
extern void outnu (unsigned);
extern void outs (char *);
extern void outsn (char *, char len);
extern void terpri (void);
extern void fresh_line (void);

extern void errouts (char *);

#endif // #ifndef __LIBLISP_IO_H__
