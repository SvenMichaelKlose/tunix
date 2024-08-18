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
    signed char   (*err)    (void);
    char          (*conin)  (void);
    char          (*in)     (void);
    void FASTCALL (*out)    (char c);
    void FASTCALL (*setin)  (simpleio_chn_t);
    void FASTCALL (*setout) (simpleio_chn_t);
    void FASTCALL (*close)  (simpleio_chn_t);
} simpleio;

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif

extern simpleio_chn_t fnin;     // Input file number.
extern simpleio_chn_t fnout;    // Output file number.
extern char           last_in;  // Last input char.
extern char           last_out; // Last output char.

#ifdef __CC65__
#pragma zpsym ("fnin")
#pragma zpsym ("fnout")
#pragma zpsym ("last_in")
#pragma zpsym ("last_out")
#pragma bss-name (pop)
#endif

extern bool             eof         (void);
extern signed char      err         (void);
    
extern void FASTCALL    setin       (simpleio_chn_t fn);
extern void FASTCALL    setout      (simpleio_chn_t fn);

extern char             conin       (void);
extern char             in          (void);
extern void             putback     (void);
extern void             skip_spaces (void);
extern void FASTCALL    inm         (char *, size_t);
extern void FASTCALL    out         (char c);
extern void FASTCALL    outnu       (unsigned long);
extern void FASTCALL    outn        (long);
extern void FASTCALL    outhn       (unsigned char);
extern void FASTCALL    outhb       (unsigned char);
extern void FASTCALL    outhw       (unsigned);
extern void FASTCALL    outs        (char *);
extern void FASTCALL    outsn       (char *, char len);
extern void FASTCALL    outm        (char *, size_t);
extern void             terpri      (void);
extern void             fresh_line  (void);

extern simpleio_chn_t   FASTCALL  simpleio_open  (char * pathname, char mode);
extern void             FASTCALL  simpleio_close (simpleio_chn_t);

extern void             simpleio_init (void);
extern void FASTCALL    simpleio_set  (simpleio *);

#endif // #ifndef __LIBSIMPLEIO_H__
