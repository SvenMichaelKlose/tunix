#ifndef __LIBLISP_H__
#define __LIBLISP_H__

#define STACK_SIZE  1024

typedef unsigned char uchar;

#ifdef CC65
#define FASTCALL    __fastcall__
#else
#define FASTCALL
#endif

typedef void * lispptr;
typedef lispptr FASTCALL (*builtin_fun) (lispptr);

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

extern lispptr universe;
extern char * stack_start;

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern lispptr t;
extern lispptr quote;
extern char * heap_start;
extern char * heap_free;
extern char * heap_end;
extern char * stack;
extern char * stack_end;
#ifdef __CC65__
#pragma zpsym ("t")
#pragma zpsym ("quote")
#pragma zpsym ("heap_start")
#pragma zpsym ("heap_free")
#pragma zpsym ("heap_end")
#pragma zpsym ("stack")
#pragma zpsym ("stack_end")
#pragma bss-name (pop)
#endif

#define nil 0

#define EXPAND_UNIVERSE(x) \
    do { \
        PUSH(x); \
        universe = lisp_make_cons (x, universe); \
        POP(x); \
    } while (0)
#define PUSH(x) \
    do { \
        stack -= sizeof (lispptr); \
        *(lispptr *) stack = x; \
    } while (0)
#define POP(x) \
    do { \
        x = *(lispptr *) stack; \
        stack += sizeof (lispptr); \
    } while (0)

#define TYPE_NAMED    64
#define TYPE_MARKED   128
#define TYPE_MASK     (7 | TYPE_NAMED)
#define TYPE_CONS     1
#define TYPE_NUMBER   2
#define TYPE_SYMBOL   (3 | TYPE_NAMED)
#define TYPE_BUILTIN  (4 | TYPE_NAMED)
#define TYPE_MAX      4

#define PTRTYPE(x)  (*((char *) (x)))
#define TYPE(x)     (PTRTYPE(x) & TYPE_MASK)

#define MARKED(x)   (PTRTYPE(x) & TYPE_MARKED)
#define MARK(x)     (PTRTYPE(x) |= TYPE_MARKED)
#define UNMARK(x)   (PTRTYPE(x) &= ~TYPE_MARKED)

#define CONS(x)      ((cons *) (x))
#define CAR(x)       (CONS(x)->car)
#define CDR(x)       (CONS(x)->cdr)
#define LIST_CAR(x)  (!(x) ? x : CAR(x))
#define LIST_CDR(x)  (!(x) ? x : CDR(x))
#define RPLACA(v, x) (CAR(x) = v)
#define RPLACD(v, x) (CDR(x) = v)

#define BOOL(x)      ((x) ? t : nil)
#define CONSP(x)     (TYPE(x) == TYPE_CONS)
#define ATOM(x)      (TYPE(x) != TYPE_CONS)
#define LISTP(x)     (!(x) || CONSP(x))
#define NUMBERP(x)   (TYPE(x) == TYPE_NUMBER)
#define SYMBOLP(x)   (TYPE(x) == TYPE_SYMBOL)
#define BUILTINP(x)  (TYPE(x) == TYPE_BUILTIN)

#define NUMBER(n)              ((number *) (n))
#define NUMBER_VALUE(n)        (NUMBER(n)->value)
#define SET_NUMBER_VALUE(n, x) (NUMBER(n)->value = x)

#define SYMBOL(s)              ((symbol *) (s))
#define SYMBOL_VALUE(s)        (SYMBOL(s)->value)
#define SET_SYMBOL_VALUE(s, x) (SYMBOL(s)->value = x)

#define FUNARGS(x)      CAR(x)
#define FUNBODY(x)      CDR(x)

extern lispptr FASTCALL lisp_make_cons (lispptr, lispptr);
extern lispptr FASTCALL lisp_make_number (int);
extern lispptr FASTCALL lisp_make_symbol (char *, uchar len);
extern lispptr lisp_read (void);
extern lispptr lisp_print (lispptr);

extern lispptr eval_list (lispptr);
extern lispptr eval_body (lispptr);
extern lispptr apply (lispptr fun, lispptr args, bool do_eval);
extern lispptr eval (lispptr);

extern void    gc (void);

extern unsigned objsize (char *);

extern void    lisp_init (void);
extern void    add_builtins (struct builtin *);

#endif // #ifndef __LIBLISP_H__
