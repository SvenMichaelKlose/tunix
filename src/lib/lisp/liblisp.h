#ifndef __LIBLISP_H__
#define __LIBLISP_H__

#define HEAP_START  0x5000

typedef void * lispptr;
typedef unsigned char uchar;

#define TYPE_NAMED    64
#define TYPE_MARKED   128
#define TYPE_MASK     7
#define TYPE_CONS     1
#define TYPE_NUMBER   2
#define TYPE_SYMBOL   (3 | TYPE_NAMED)
#define TYPE_BUILTIN  (4 | TYPE_NAMED)

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

extern uchar lisp_sizes[];

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

#define CAR(x)       (((cons *) x)->car)
#define CDR(x)       (((cons *) x)->cdr)
#define RPLACA(v, x) (x->car = v)
#define RPLACD(v, x) (x->cdr = v)

#define NOT(x)      (x == nil)
#define PTRTYPE(x)  (*(char *) x)
#define TYPE(x)     (*(char *) x & TYPE_MASK)
#define CONSP(x)    (TYPE(x) == TYPE_CONS)
#define ATOM(x)     (TYPE(x) != TYPE_CONS)
#define LISTP(x)    (NOT(x) || CONSP(x))
#define NUMBERP(x)  (TYPE(x) == TYPE_NUMBER)
#define SYMBOLP(x)  (TYPE(x) == TYPE_SYMBOL)
#define BUILTINP(x) (TYPE(x) == TYPE_BUILTIN)
#define OBJSIZE(x) \
    ((PTRTYPE(x) & TYPE_NAMED) ? \
        lisp_sizes[*(char *)x] + ((symbol *)x)->len : \
        lisp_sizes[*(char *)x])

#define NUMBER_VALUE(n)        (((number *) n)->value)
#define SET_NUMBER_VALUE(n, x) (((number *) n)->value = x)

#define SYMBOL_VALUE(s)        (((symbol *) s)->value)
#define SET_SYMBOL_VALUE(s, x) (((symbol *) s)->value = x)

#define FUNBODY(x)      CAR(x)
#define FUNARGS(x)      CDR(x)

lispptr __fastcall__ lisp_make_cons (lispptr car, lispptr cdr);
lispptr __fastcall__ lisp_make_number (int x);
lispptr __fastcall__ lisp_make_symbol (char * str, uchar len);
lispptr              lisp_read (void);
void                 lisp_print (lispptr x);
void                 lisp_init (void);

#endif // #ifndef __LIBLISP_H__
