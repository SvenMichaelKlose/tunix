#ifndef __LIBLISP_H__
#define __LIBLISP_H__

typedef unsigned char uchar;

#ifdef CC65
#define FASTCALL    __fastcall__
#else
#define FASTCALL
#endif

typedef void * lispptr;
typedef lispptr FASTCALL (*builtin_fun) (lispptr);

#define TYPE_NAMED    64
#define TYPE_MARKED   128
#define MASK_MARKED   127
#define TYPE_MASK     (7 + TYPE_NAMED)
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

struct builtin {
    char *       name;
    builtin_fun  func;
};

extern uchar lisp_sizes[];

extern lispptr universe;

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern lispptr nil;
extern lispptr t;
extern char * heap_start;
extern char * heap_free;
extern char * heap_end;
#ifdef __CC65__
#pragma zpsym ("nil")
#pragma zpsym ("t")
#pragma zpsym ("heap_start")
#pragma zpsym ("heap_free")
#pragma zpsym ("heap_end")
#pragma bss-name (pop)
#endif

#define MARKED(x)   (PTRTYPE(x) & TYPE_MARKED)
#define MARK(x)     (PTRTYPE(x) |= TYPE_MARKED)

#define NOT(x)      (nil == (lispptr) (x))

#define CONS(x)      ((cons *) (x))
#define CAR(x)       (CONS(x)->car)
#define CDR(x)       (CONS(x)->cdr)
#define LIST_CAR(x)  (NOT(CAR(x)) ? x : CAR(x))
#define LIST_CDR(x)  (NOT(CDR(x)) ? x : CDR(x))
#define RPLACA(v, x) (CAR(x) = v)
#define RPLACD(v, x) (CDR(x) = v)

#define BOOL(x)      ((x) ? t : nil)
#define PTRTYPE(x)   (*((char *) (x)))
#define TYPE(x)      (PTRTYPE(x) & TYPE_MASK)
#define CONSP(x)     (TYPE(x) == TYPE_CONS)
#define ATOM(x)      (TYPE(x) != TYPE_CONS)
#define LISTP(x)     (NOT(x) || CONSP(x))
#define NUMBERP(x)   (TYPE(x) == TYPE_NUMBER)
#define SYMBOLP(x)   (TYPE(x) == TYPE_SYMBOL)
#define BUILTINP(x)  (TYPE(x) == TYPE_BUILTIN)
#define OBJSIZE(x) \
    ((PTRTYPE(x) & TYPE_NAMED) ? \
        lisp_sizes[*(char *)x] + ((symbol *) x)->len : \
        lisp_sizes[*(char *)x])

#define NUMBER(n)              ((number *) (n))
#define NUMBER_VALUE(n)        (NUMBER(n)->value)
#define SET_NUMBER_VALUE(n, x) (NUMBER(n)->value = x)

#define SYMBOL(s)              ((symbol *) (s))
#define SYMBOL_VALUE(s)        (SYMBOL(s)->value)
#define SET_SYMBOL_VALUE(s, x) (SYMBOL(s)->value = x)

#define FUNBODY(x)      CAR(x)
#define FUNARGS(x)      CDR(x)

extern lispptr __fastcall__ lisp_make_cons (lispptr, lispptr);
extern lispptr __fastcall__ lisp_make_number (int);
extern lispptr __fastcall__ lisp_make_symbol (char *, uchar len);
extern lispptr lisp_read (void);
extern lispptr lisp_print (lispptr);

extern lispptr eval_list (lispptr x);
extern lispptr eval_body (lispptr x);
extern lispptr apply (lispptr fun, lispptr args, bool do_eval);
extern lispptr eval (lispptr x);

extern void    lisp_init (void);
extern void    add_builtins (struct builtin *);

#endif // #ifndef __LIBLISP_H__
