#ifndef __LIBSIMPLEIO_H__
#define __LIBSIMPLEIO_H__

#define STDIN  0
#ifdef __CC65__
    #define STDOUT 3
    #define STDERR 3
#else
    #define STDOUT 1
    #define STDERR 2
#endif

typedef signed char simpleio_chn_t;

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
extern simpleio_chn_t fnin;   // Input file number.
extern simpleio_chn_t fnout;  // Output file number.
extern char last_in;   // Last input char.
extern char last_out;  // Last output char.
#ifdef __CC65__
#pragma zpsym ("fnin")
#pragma zpsym ("fnout")
#pragma zpsym ("last_in")
#pragma zpsym ("last_out")
#pragma bss-name (pop)
#endif

extern char eof (void);
extern char err (void);
    
extern void setin  (simpleio_chn_t fn);
extern void setout (simpleio_chn_t fn);

extern char in          (void);      // Read char.
extern void putback     (void); // Put back char for in().
extern void skip_spaces (void);

extern void out         (char c);
extern void outnu       (unsigned long);
extern void outn        (long);
extern void outs        (char *);
extern void outsn       (char *, char len);
extern void terpri      (void);
extern void fresh_line  (void);

extern simpleio_chn_t simpleio_open (char * pathname, char mode);
extern void           simpleio_close (simpleio_chn_t);

extern void simpleio_init ();
extern void simpleio_set (simpleio *);

#endif // #ifndef __LIBSIMPLEIO_H__
