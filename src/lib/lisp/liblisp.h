#ifndef __LIBLISP_H__
#define __LIBLISP_H__

#define HEAP_START  0x5000

typedef void * lispptr;
typedef unsigned char uchar;

#define TYPE_CONS     1
#define TYPE_NUMBER   2
#define TYPE_SYMBOL   4
#define TYPE_BUILTIN  8

#define PTRTYPE(x)  (((char *) x)[1])
#define CONSP(x)    (PTRTYPE(x) & TYPE_CONS)
#define NOTP(x)     (x == nil)

typedef struct _cons {
    uchar    size;
    uchar    type;
    lispptr  car;
    lispptr  cdr;
} cons;

typedef struct _number {
    uchar   size;
    uchar   type;
    int     value;
} number;

typedef struct _symbol {
    uchar   size;
    uchar   type;
    lispptr value;
    lispptr bind;
    uchar   len;
    uchar   name;
} symbol;

extern lispptr nil;
extern lispptr t;

lispptr __fastcall__ make_cons (lispptr car, lispptr cdr);
lispptr __fastcall__ make_number (int x);
lispptr __fastcall__ make_symbol (char * str, uchar len);
lispptr              read (void);
void                 print (lispptr x);

#endif // #ifndef __LIBLISP_H__
