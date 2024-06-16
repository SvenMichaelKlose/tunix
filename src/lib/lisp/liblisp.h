#ifndef __LIBLISP_H__
#define __LIBLISP_H__

#define MAX_SYMBOL  255

// Give inappropriately happy developers a hard time.
//#define GC_STRESS
// Print message if garbage collector takes action.
#define VERBOSE_GC
//#define VERBOSE_EVAL
//#define VERBOSE_LOAD

// # Compile-time option SLOW
//
// **Use only when desperate for smaller code size!**
// Use functions instead of expressions to save code space
// at the expense of performance (almost halved).
//#define SLOW

#ifndef STACK_SIZE
    #ifdef __CC65__
        #define STACK_SIZE  2048
    #else
        #define STACK_SIZE  (64 * 1024)
    #endif
#endif
#define TAGSTACK_SIZE  1024
#define MIN_RELOC_TABLE_ENTRIES  64

typedef unsigned char uchar;
typedef long lispnum_t;

#ifdef CC65
#define FASTCALL    __fastcall__
#else
#define FASTCALL
#endif

typedef void * lispptr;
typedef lispptr (*builtin_fun) (void);

typedef struct _cons {
    uchar    type;
    lispptr  car;
    lispptr  cdr;
} cons;

typedef struct _number {
    uchar      type;
    lispnum_t  value;
} number;

typedef struct _symbol {
    uchar    type;
    lispptr  value;
    uchar    length;
} symbol;

struct builtin {
    char *       name;
    char *       argdef;
    builtin_fun  func;
};

extern lispptr universe;
extern lispptr stdin;
extern lispptr stdout;
extern char * stack_start;
extern char buffer[MAX_SYMBOL + 1];
extern struct builtin builtins[];
extern lispptr last_eval_expr;
extern char *  last_errstr;
extern bool    debug_mode;

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern lispptr t;
extern lispptr quote;
extern lispptr quasiquote;
extern lispptr unquote;
extern lispptr unquote_spliced;
extern char * heap_start;
extern char * heap_free;
extern char * heap_end;
extern lispptr args;
extern char * stack;
extern char * stack_end;
extern char * tagstack_start;
extern char * tagstack_end;
extern char * tagstack;
extern char * msg;
extern bool has_error;     // Return to REPL.
extern bool unevaluated;    // Tell eval0() to not evaluate arguments.
extern lispptr block_sym;
extern lispptr return_sym;
extern lispptr return_name;
extern lispptr return_value;
extern lispptr go_sym;
extern lispptr go_tag;
extern lispptr arg1;
extern lispptr arg2;
extern lispptr arg2c;
extern lispptr delayed_eval;
#ifdef __CC65__
#pragma zpsym ("t")
#pragma zpsym ("quote")
#pragma zpsym ("quasiquote")
#pragma zpsym ("unquote")
#pragma zpsym ("unquote_spliced")
#pragma zpsym ("heap_start")
#pragma zpsym ("heap_free")
#pragma zpsym ("heap_end")
#pragma zpsym ("args")
#pragma zpsym ("stack")
#pragma zpsym ("stack_end")
#pragma zpsym ("tagstack_start")
#pragma zpsym ("tagstack_end")
#pragma zpsym ("tagstack")
#pragma zpsym ("msg")
#pragma zpsym ("has_error")
#pragma zpsym ("unevaluated")
#pragma zpsym ("arg1")
#pragma zpsym ("arg2")
#pragma zpsym ("arg2c")
#pragma zpsym ("block_sym")
#pragma zpsym ("return_sym")
#pragma zpsym ("return_value")
#pragma zpsym ("return_name")
#pragma zpsym ("go_sym")
#pragma zpsym ("go_tag")
#pragma zpsym ("delayed_eval")
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
        if (stack == stack_start) \
            stack_overflow (); \
        stack -= sizeof (lispptr); \
        *(lispptr *) stack = x; \
    } while (0)
#define POP(x) \
    do { \
        if (stack == stack_end) \
            stack_underflow (); \
        x = *(lispptr *) stack; \
        stack += sizeof (lispptr); \
    } while (0)

#define PUSH_TAG(x) \
    do { \
        (*--tagstack = (x)); \
    } while (0)
#define POP_TAG(x) \
    do { \
        ((x) = *tagstack++); \
    } while (0)
#define PUSH_TAGW(x) \
    do { \
        if (tagstack == tagstack_start) \
            tagstack_overflow (); \
        tagstack -= sizeof (lispptr); \
        *(lispptr *) tagstack = x; \
    } while (0)
#define POP_TAGW(x) \
    do { \
        if (tagstack == tagstack_end) \
            tagstack_underflow (); \
        x = *(lispptr *) tagstack; \
        tagstack += sizeof (lispptr); \
    } while (0)

#define TAG_DONE            0
#define TAG_BARG_NEXT       1
#define TAG_ARG_NEXT        2
#define TAG_CONTINUE_BODY   3
#define TAG_CONTINUE_BLOCK  4

#define DOLIST(x, init) \
    for (x = init; x; x = CDR(x))
#define TYPESAFE_DOLIST(x, init) \
    for (x = init; x; x = LIST_CDR(x))

#define TYPE_MARKED   128
#define TYPE_NAMED    64
#define TYPE_SPECIAL  32    // User-defined function with no argument evaluation.

#define TYPE_MASK     (7 | TYPE_NAMED)

#define TYPE_CONS     1
#define TYPE_NUMBER   2
#define TYPE_SYMBOL   (3 | TYPE_NAMED)
#define TYPE_BUILTIN  (4 | TYPE_NAMED)
#define TYPE_MAX      4

#define PTRTYPE(x)  (*((char *) (x))) // TODO: Rename.
#define TYPE(x)     (PTRTYPE(x) & TYPE_MASK)

#define MARKED(x)   (!(x) || PTRTYPE(x) & TYPE_MARKED)
#define MARK(x)     (PTRTYPE(x) |= TYPE_MARKED)
#define UNMARK(x)   (PTRTYPE(x) &= ~TYPE_MARKED)

#define CONS(x)     ((cons *) (x))

#ifdef SLOW

#define CAR(x)       (lisp_car (x))
#define CDR(x)       (lisp_cdr (x))
#define ATOM(x)      (lisp_atom (x))
#define CONSP(x)     (lisp_consp (x))
#define LISTP(x)     (lisp_listp (x))
#define NUMBERP(x)   (lisp_numberp (x))
#define SYMBOLP(x)   (lisp_symbolp (x))
#define BUILTINP(x)  (lisp_builtinp (x))
#define SPECIALP(x)  (lisp_specialp (x))
extern lispptr FASTCALL lisp_car (lispptr);
extern lispptr FASTCALL lisp_cdr (lispptr);
extern bool FASTCALL lisp_atom (lispptr);
extern bool FASTCALL lisp_consp (lispptr);
extern bool FASTCALL lisp_listp (lispptr);
extern bool FASTCALL lisp_numberp (lispptr);
extern bool FASTCALL lisp_symbolp (lispptr);
extern bool FASTCALL lisp_builtinp (lispptr);
extern bool FASTCALL lisp_specialp (lispptr);

#else // #ifdef SLOW

#define CAR(x)       (CONS(x)->car)
#define CDR(x)       (CONS(x)->cdr)
#define ATOM(x)      (!(x) || TYPE(x) != TYPE_CONS)
#define CONSP(x)     ((x) && TYPE(x) == TYPE_CONS)
#define LISTP(x)     (!(x) || TYPE(x) == TYPE_CONS)
#define NUMBERP(x)   ((x) && TYPE(x) == TYPE_NUMBER)
#define SYMBOLP(x)   (!(x) || TYPE(x) == TYPE_SYMBOL)
#define BUILTINP(x)  ((x) && TYPE(x) == TYPE_BUILTIN)
#define SPECIALP(x)  ((x) && PTRTYPE(x) & TYPE_SPECIAL)

#endif // #ifdef SLOW

#define LIST_CAR(x)  (!(x) ? x : CAR(x))
#define LIST_CDR(x)  (!(x) ? x : CDR(x))
#define SETCAR(x, v) (CONS(x)->car = v)
#define SETCDR(x, v) (CONS(x)->cdr = v)

#define BOOL(x)      ((x) ? t : nil)

#define NUMBER(n)              ((number *) (n))
#define NUMBER_VALUE(n)        (NUMBER(n)->value)
#define SET_NUMBER_VALUE(n, x) (NUMBER(n)->value = x)

#define SYMBOL(s)              ((symbol *) (s))
#define SYMBOL_VALUE(s)        (SYMBOL(s)->value)
#define SYMBOL_LENGTH(s)       (SYMBOL(s)->length)
#define SYMBOL_NAME(s) \
    ((char *) s + sizeof (symbol))
#define SET_SYMBOL_VALUE(s, x) (SYMBOL(s)->value = x)

#define FUNARGS(x)      CAR(x)
#define FUNBODY(x)      CDR(x)

extern void  stack_overflow (void);
extern void  stack_underflow (void);
extern void  tagstack_overflow (void);
extern void  tagstack_underflow (void);

extern lispptr  FASTCALL lisp_make_cons (lispptr, lispptr);
extern lispptr  FASTCALL lisp_make_number (lispnum_t);
extern lispptr  FASTCALL lisp_alloc_symbol (char *, uchar len);
extern lispptr  FASTCALL lisp_make_symbol (char *, uchar len);
extern lispptr  lisp_read (void);
extern lispptr  FASTCALL lisp_print (lispptr);

// Arguments in global 'x'.
extern lispptr  eval0 (void);
extern lispptr  eval (void);
extern lispptr  eval_list (void);
extern lispptr  funcall (void);

extern void     FASTCALL error (char * msg);

extern void     gc (void);
extern unsigned FASTCALL objsize (char *);

extern lispptr  lisp_repl (void);
extern bool     lisp_init (void);
extern void     add_builtins (struct builtin *);

#endif // #ifndef __LIBLISP_H__
