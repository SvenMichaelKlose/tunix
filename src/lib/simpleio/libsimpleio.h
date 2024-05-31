#ifndef __LIBSIMPLEIO_H__
#define __LIBSIMPLEIO_H__

#define STDIN  0
#define STDOUT 3

typedef char simpleio_chn_t;

// Supplied by driver libraries.
typedef struct _simpleio {
    bool (*eof)    (void);
    char (*err)    (void);
    char (*in)     (void);
    void (*out)    (char c);
    void (*setin)  (simpleio_chn_t);
    void (*setout) (simpleio_chn_t);
    void (*close)  (simpleio_chn_t);
} simpleio;

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
extern void setin (simpleio_chn_t fn);
extern void setout (simpleio_chn_t fn);

extern char in (void);      // Read char.
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

extern void simpleio_open (simpleio_chn_t, char * pathname, char mode);
extern void simpleio_close (simpleio_chn_t);
extern void simpleio_init ();
extern void simpleio_set (simpleio *);

#endif // #ifndef __LIBSIMPLEIO_H__
