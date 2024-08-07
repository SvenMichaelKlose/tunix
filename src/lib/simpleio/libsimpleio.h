#ifndef __LIBSIMPLEIO_H__
#define __LIBSIMPLEIO_H__

#ifdef __CC65__
    #ifndef FASTCALL
    #define FASTCALL __fastcall__
    #endif
#else
    #ifndef FASTCALL
    #define FASTCALL
    #endif
#endif

#define STDIN  0
#define STDOUT 1
#define STDERR 2

typedef signed char simpleio_chn_t;

// Supplied by driver libraries.
typedef struct _simpleio {
    bool          (*eof)    (void);
    char          (*err)    (void);
    char          (*in)     (void);
    void FASTCALL (*out)    (char c);
    void FASTCALL (*setin)  (simpleio_chn_t);
    void FASTCALL (*setout) (simpleio_chn_t);
    void FASTCALL (*close)  (simpleio_chn_t);
} simpleio;

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern simpleio_chn_t fnin;   // Input file number.
extern simpleio_chn_t fnout;  // Output file number.
extern char last_in;          // Last input char.
extern char last_out;         // Last output char.
#ifdef __CC65__
#pragma zpsym ("fnin")
#pragma zpsym ("fnout")
#pragma zpsym ("last_in")
#pragma zpsym ("last_out")
#pragma bss-name (pop)
#endif

extern char eof (void);
extern char err (void);
    
extern void FASTCALL    setin  (simpleio_chn_t fn);
extern void FASTCALL    setout (simpleio_chn_t fn);

extern char in          (void);  // Read char.
extern void putback     (void);  // Put back char for in().
extern void skip_spaces (void);

extern void FASTCALL    out         (char c);
extern void             outnu       (unsigned long);
extern void             outn        (long);
extern void FASTCALL    outs        (char *);
extern void FASTCALL    outsn       (char *, char len);
extern void             terpri      (void);
extern void             fresh_line  (void);

extern simpleio_chn_t FASTCALL  simpleio_open  (char * pathname, char mode);
extern void           FASTCALL  simpleio_close (simpleio_chn_t);

extern void             simpleio_init ();
extern void FASTCALL    simpleio_set  (simpleio *);

#endif // #ifndef __LIBSIMPLEIO_H__
