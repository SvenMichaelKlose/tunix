#ifndef __LIBLISP_H__
#define __LIBLISP_H__

#ifdef RELEASE
    #define NDEBUG
    #define VERBOSE_GC
#else
    #define GCSTACK_CHECKS
    #define TAGSTACK_CHECKS
    #define VERBOSE_GC
#endif // #ifdef RELEASE

#ifdef TARGET_C16
    #define SLOW
#endif // #ifdef TARGET_C16
#ifdef TARGET_VIC20
    #define SLOW
#endif // #ifdef TARGET_VIC20

// Disable C compiler level debugging.
//#define NDEBUG

// Maximum symbol length.
#define MAX_SYMBOL  255

// Print message if garbage collector takes action.
//#define VERBOSE_GC

// Real object at address 0 to get around addiitonal
// pointer checks.  PLANNED!
//#define NULLOBJ

// Give inappropriately happy developers a hard time.
//#define GC_STRESS

// Print current expression to eval().
//#define VERBOSE_EVAL

// Print LOADed expressions before evaluation.
//#define VERBOSE_LOAD

// Disable calling user function ONERROR on errors.
//#define NO_ONERROR

// Inline less code, half performance.
//#define SLOW

// Do boundary checks of tag and GC stack pointers before
// moving them.
//#define GCSTACK_CHECKS
//#define TAGSTACK_CHECKS

// Print 'x instead of (quote x).
#define PRINT_SHORT_QUOTES

// Do not print anonymous symbols.
//#define NO_PRINT_ANONYMOUS

// No error handling.
// ~20% less code size with cc65.
//#define NAIVE

// GC sweep: do not copy if object didn't move.
// Adds extra code.
#define SKIPPING_SWEEP

// Multiple heaps.
//#define FRAGMENTED_HEAP

// Use malloc() to allocate the heap.
#define MALLOCD_HEAP

// Use malloc() to allocate the tag stack.
// Will be taken from the heap otherwise.
//#define MALLOCD_TAGSTACK

// Use malloc() to allocate the object stack.
//#define MALLOCD_STACK

#ifdef __CC65__
#define MALLOCD_STACK
#endif

#ifdef TARGET_C128
#define HEAP_SIZE   (24 * 1024U)
#define MALLOCD_STACK
#endif

#ifdef TARGET_C16
#define HEAP_SIZE   (10 * 1024U)
#define MALLOCD_STACK
#endif

#ifdef TARGET_C64
#define HEAP_SIZE   (10 * 1024U)
#define MALLOCD_STACK
#endif

#ifdef TARGET_PET
#define HEAP_SIZE   (10 * 1024U)
#define MALLOCD_STACK
#endif

#ifdef TARGET_PLUS4
#define HEAP_SIZE   (32 * 1024U)
#define MALLOCD_STACK
#endif

#ifdef TARGET_VIC20
#define HEAP_SIZE   (32 * 1024U)
#define FRAGMENTED_HEAP
#endif

#ifdef TARGET_UNIX
#define HEAP_SIZE   (128 * 1024U)
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#endif

#if !defined (MALLOCD_HEAP) && !defined (HEAP_SIZE)
    #error "Please specify HEAP_SIZE with no MALLOCD_HEAP."
#endif

#define STACK_SIZE               (HEAP_SIZE / 64U)
#define TAGSTACK_SIZE            (HEAP_SIZE / 64U)
#define MIN_RELOC_TABLE_ENTRIES  (HEAP_SIZE / 128U)

#if defined(NO_DEBUGGER) && !defined(NO_ONERROR)
#define NO_ONERROR
#endif

#if defined(NAIVE) && !defined(NO_DEBUGGER)
#define NO_DEBUGGER
#endif
#if defined(NAIVE) && !defined(NDEBUG)
#define NDEBUG
#endif

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
    lispptr  next;
    uchar    length;
} symbol;

struct builtin {
    char *       name;
    char *       argdef;
    builtin_fun  func;
};

struct heap_fragment {
    char *  start;
    char *  free;
    char *  end;
};

extern lispptr universe;
extern char * stack_start;
extern char buffer[MAX_SYMBOL + 1];
extern struct builtin builtins[];
extern lispptr current_expr;
extern lispptr current_toplevel;
extern char *  last_errstr;
extern bool    debug_mode;
extern lispptr first_symbol;
extern lispptr last_symbol;
extern lispptr highlighted;
extern bool    do_highlight;
extern lispptr onerror_sym;

extern lispptr t;
extern lispptr quote;
extern lispptr quasiquote;
extern lispptr unquote;
extern lispptr unquote_spliced;

extern char num_repls;
extern bool do_break_repl;
extern bool do_continue_repl;
extern bool do_exit_program;

extern lispptr list_start;
extern lispptr list_last;

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern lispptr tmp;
extern lispptr tmp2;
extern char tmpc;
extern char * heap_start;
extern char * heap_free;
extern char * heap_end;
extern lispptr args;
extern char * stack;
extern char * stack_end;
extern char * tagstack_start;
extern char * tagstack_end;
extern char * tagstack;
extern char * badef;
extern char has_error;
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
extern lispptr debug_step;
extern bool do_invoke_debugger;
#ifdef __CC65__
#pragma zpsym ("tmp")
#pragma zpsym ("tmp2")
#pragma zpsym ("tmpc")
#pragma zpsym ("heap_start")
#pragma zpsym ("heap_free")
#pragma zpsym ("heap_end")
#pragma zpsym ("args")
#pragma zpsym ("stack")
#pragma zpsym ("stack_end")
#pragma zpsym ("tagstack_start")
#pragma zpsym ("tagstack_end")
#pragma zpsym ("tagstack")
#pragma zpsym ("badef")
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
#pragma zpsym ("debug_step")
#pragma zpsym ("do_invoke_debugger")
#pragma bss-name (pop)
#endif

#define nil 0

#ifdef GCSTACK_CHECKS
#define STACK_CHECK_OVERFLOW() \
        if (stack == stack_start) \
            stack_overflow ()
#define STACK_CHECK_UNDERFLOW() \
        if (stack == stack_end) \
            stack_underflow ()
#else // #ifdef GCSTACK_CHECKS
#define STACK_CHECK_OVERFLOW()
#define STACK_CHECK_UNDERFLOW()
#endif // #ifdef GCSTACK_CHECKS

#ifdef TAGSTACK_CHECKS
#define TAGSTACK_CHECK_OVERFLOW() \
        if (tagstack == tagstack_start) \
            tagstack_overflow ()
#define TAGSTACK_CHECK_UNDERFLOW() \
        if (tagstack == tagstack_end) \
            tagstack_underflow ()
#else // #ifdef TAGSTACK_CHECKS
#define TAGSTACK_CHECK_OVERFLOW()
#define TAGSTACK_CHECK_UNDERFLOW()
#endif // #ifdef TAGSTACK_CHECKS

#ifdef SLOW
#define PUSH(x)         pushgc (x)
#define POP(x)          do { x = popgc (); } while (0)
#define PUSH_TAG(x)     pushtag (x)
#define POP_TAG(x)      do { x = poptag (); } while (0)
#define PUSH_TAGW(x)    pushtagw (x)
#define POP_TAGW(x)     do { x = poptagw (); } while (0)
extern void     FASTCALL pushgc (lispptr);
extern lispptr           popgc (void);
extern void     FASTCALL pushtag (char);
extern char              poptag (void);
extern void     FASTCALL pushtagw (lispptr);
extern lispptr           poptagw (void);
#else // #ifdef SLOW
#define PUSH(x) \
    do { \
        STACK_CHECK_OVERFLOW(); \
        stack -= sizeof (lispptr); \
        *(lispptr *) stack = x; \
    } while (0)
#define POP(x) \
    do { \
        STACK_CHECK_UNDERFLOW(); \
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
        TAGSTACK_CHECK_OVERFLOW(); \
        tagstack -= sizeof (lispptr); \
        *(lispptr *) tagstack = x; \
    } while (0)
#define POP_TAGW(x) \
    do { \
        TAGSTACK_CHECK_UNDERFLOW(); \
        x = *(lispptr *) tagstack; \
        tagstack += sizeof (lispptr); \
    } while (0)
#endif // #ifdef SLOW

#define TAG_DONE                  0
#define TAG_NEXT_BUILTIN_ARG      1
#define TAG_NEXT_ARG              2
#define TAG_NEXT_BODY_STATEMENT   3
#define TAG_NEXT_BLOCK_STATEMENT  4

#define DOLIST(x, init) \
    for (x = init; CONSP(x); x = CDR(x))
#define TYPESAFE_DOLIST(x, init) \
    for (x = init; CONSP(x); x = LIST_CDR(x))

#define TYPE_CONS       1
#define TYPE_NUMBER     2
#define TYPE_SYMBOL     4
#define TYPE_BUILTIN    8
#define TYPE_EXTENDED   16
#define TYPE_MASK       31
#define TYPE_UNUSED1    32
#define TYPE_UNUSED2    64
#define TYPE_MARKED     128
#define TYPE_SPECIAL    (TYPE_SYMBOL | TYPE_EXTENDED)

#define TYPE(x)     (*((char *) (x))) // TODO: Rename.
#define TYPEBITS(x) (TYPE(x) & TYPE_MASK)

#define MARKED(x)   (!(x) || TYPE(x) & TYPE_MARKED)
#define MARK(x)     (TYPE(x) |= TYPE_MARKED)
#define UNMARK(x)   (TYPE(x) &= ~TYPE_MARKED)

#define CONS(x)     ((cons *) (x))

#define _ATOM(x)        (!(x) || !(TYPE(x) & TYPE_CONS))
#define _CONSP(x)       ((x) && (TYPE(x) & TYPE_CONS))
#define _SYMBOLP(x)     (!(x) || (TYPE(x) & TYPE_SYMBOL))
#define _BUILTINP(x)    ((x) && (TYPE(x) & TYPE_BUILTIN))
#define _NUMBERP(x)     ((x) && (TYPE(x) & TYPE_NUMBER))
#define _LISTP(x)       (!(x) || (TYPE(x) & TYPE_CONS))
#define _SPECIALP(x)    ((x) && (TYPE(x) & TYPE_SPECIAL) == TYPE_SPECIAL)
#define _NAMEDP(x)      ((x) && TYPE(x) & (TYPE_SYMBOL | TYPE_BUILTIN))

#define EXTENDEDP(x) ((x) && (TYPE(x) & TYPE_EXTENDED))

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
extern bool    FASTCALL lisp_atom (lispptr);
extern bool    FASTCALL lisp_consp (lispptr);
extern bool    FASTCALL lisp_listp (lispptr);
extern bool    FASTCALL lisp_numberp (lispptr);
extern bool    FASTCALL lisp_symbolp (lispptr);
extern bool    FASTCALL lisp_builtinp (lispptr);
extern bool    FASTCALL lisp_specialp (lispptr);
#else // #ifdef SLOW
#define CAR(x)       (CONS(x)->car)
#define CDR(x)       (CONS(x)->cdr)
#define ATOM(x)      _ATOM(x)
#define CONSP(x)     _CONSP(x)
#define LISTP(x)     _LISTP(x)
#define NUMBERP(x)   _NUMBERP(x)
#define SYMBOLP(x)   _SYMBOLP(x)
#define BUILTINP(x)  _BUILTINP(x)
#define SPECIALP(x)  _SPECIALP(x)
#endif // #ifdef SLOW

#define LIST_CAR(x)  (!(x) ? x : CAR(x))
#define LIST_CDR(x)  (!(x) ? x : CDR(x))
#define SETCAR(x, v) (CONS(x)->car = v)
#define SETCDR(x, v) (CONS(x)->cdr = v)

#define BOOL(x)      ((x) ? t : nil)

#define NUMBER(n)               ((number *) (n))
#define NUMBER_VALUE(n)         (NUMBER(n)->value)
#define SET_NUMBER_VALUE(n, x)  (NUMBER(n)->value = x)

#define SYMBOL(s)               ((symbol *) (s))
#define SYMBOL_NEXT(s)          (SYMBOL(s)->next)
#define SYMBOL_VALUE(s)         (SYMBOL(s)->value)
#define SYMBOL_LENGTH(s)        (SYMBOL(s)->length)
#define SYMBOL_NAME(s)          ((char *) s + sizeof (symbol))
#define SET_SYMBOL_NEXT(s, x)   (SYMBOL(s)->next = x)
#define SET_SYMBOL_VALUE(s, x)  (SYMBOL(s)->value = x)

#define FUNARGS(x)      CAR(x)
#define FUNBODY(x)      CDR(x)

#define ERROR_TYPE          1
#define ERROR_ARG_MISSING   2
#define ERROR_TAG_MISSING   3
#define ERROR_TOO_MANY_ARGS 4
#define ERROR_NOT_FUNCTION  5
#define ERROR_OUT_OF_HEAP   6
#define ERROR_UNKNOWN_TYPE  7
#define ERROR_NO_PAREN      8
#define ERROR_STALE_PAREN   9
#define ERROR_CHANNEL       10
#define ERROR_FILE          11
#define ERROR_USER          12
#define ERROR_INTERNAL      13

extern void     FASTCALL expand_universe (lispptr);
extern lispptr  FASTCALL make_cons (lispptr, lispptr);
extern lispptr  FASTCALL make_number (lispnum_t);
extern lispptr  FASTCALL alloc_symbol (char *, uchar len);
extern lispptr  FASTCALL make_symbol (char *, uchar len);
extern lispptr           read (void);
extern lispptr           read_symbol (void);
extern lispptr           read_number (void);
extern lispptr  FASTCALL print (lispptr);
extern lispptr  FASTCALL dprint (lispptr);
extern void     FASTCALL set_channels (simpleio_chn_t in, simpleio_chn_t out);
extern lispptr           bi_setin  (void);
extern lispptr           bi_setout (void);

// Arguments in global 'x'.
extern lispptr  eval0     (void);
extern lispptr  eval      (void);
extern lispptr  eval_list (void);
extern lispptr  funcall   (void);

extern void     gc (void);
extern unsigned FASTCALL objsize (char *);

#define REPL_STD        0
#define REPL_DEBUGGER   1
#define REPL_LOAD       2
extern lispptr  FASTCALL lisp_repl    (char mode);
extern void     FASTCALL load         (char * pathname);
extern bool              init_heap    (void);
extern void              init_eval    (void);
extern void              init_onerror (void);
extern void              init_repl    (void);
extern void     FASTCALL add_builtins (struct builtin *);
extern lispptr           debugger     (void);

#define COPY_LIST       0
#define COPY_BUTLAST    1
#define COPY_REMOVE     2
extern int      FASTCALL length    (lispptr);
extern lispptr  FASTCALL copy_list (lispptr, char mode, lispptr excluded);
extern lispptr  FASTCALL butlast   (lispptr);
extern lispptr  FASTCALL last      (lispptr);
extern lispptr  FASTCALL member    (lispptr needle, lispptr haystack);

extern void     FASTCALL internal_error      (char * msg);
extern void     FASTCALL error               (char code, char * msg);
extern void     FASTCALL err_type            (char * type, lispptr x);
extern void              stack_overflow      (void);
extern void              stack_underflow     (void);
extern void              tagstack_overflow   (void);
extern void              tagstack_underflow  (void);
extern char *   FASTCALL typestr             (lispptr * x);
extern void     FASTCALL bi_tcheck           (lispptr x, uchar type);
extern void     FASTCALL check_stacks        (char * old_stack, char * old_tagstack);
extern void              print_code_position (void);

extern void     FASTCALL name_to_buffer (lispptr s);
extern void     FASTCALL make_call      (lispptr args);
extern void     FASTCALL make_car_call  (void);

#endif // #ifndef __LIBLISP_H__
