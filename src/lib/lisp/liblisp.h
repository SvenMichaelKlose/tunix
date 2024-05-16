#ifndef __LIBLISP_H__
#define __LIBLISP_H__

#define HEAP_START  0x5000

typedef void * lispptr;
typedef unsigned char uchar;

#define TYPE_CONS     1
#define TYPE_NUMBER   2
#define TYPE_SYMBOL   3
#define TYPE_BUILTIN  4

#define NOTP(x)     (x == nil)
#define PTRTYPE(x)  (*(char *) x)
#define CONSP(x)    (PTRTYPE(x) == TYPE_CONS)
#define ATOM(x)     (PTRTYPE(x) != TYPE_CONS)
#define NUMBERP(x)  (PTRTYPE(x) == TYPE_NUMBER)
#define SYMBOLP(x)  (PTRTYPE(x) == TYPE_SYMBOL)
#define BUILTINP(x) (PTRTYPE(x) == TYPE_BUILTIN)

typedef struct _cons {
    uchar   type;
    lispptr car;
    lispptr cdr;
} cons;

typedef struct _number {
    uchar   type;
    int     value;
} number;

typedef struct _symbol {
    uchar   type;
    lispptr value;
    uchar   len;
} symbol;

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern lispptr nil;
extern lispptr t;
#ifdef __CC65__
#pragma zpsym ("nil")
#pragma zpsym ("t")
#pragma bss-name (pop)
#endif

lispptr __fastcall__ lisp_make_cons (lispptr car, lispptr cdr);
lispptr __fastcall__ lisp_make_number (int x);
lispptr __fastcall__ lisp_make_symbol (char * str, uchar len);
lispptr              lisp_read (void);
void                 lisp_print (lispptr x);
void                 lisp_init (void);

#endif // #ifndef __LIBLISP_H__
