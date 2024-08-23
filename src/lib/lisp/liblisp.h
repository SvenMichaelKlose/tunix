#ifndef __LIBLISP_H__
#define __LIBLISP_H__

//// Compile-time options
////
//// Inteded to be set via file 'src/config' or command-line.

/// Diagnostics
/// NOTE: Current output channel is used!

// Add dump_lispptr().
//#define DUMP_LISPPTR

// Dump marked objects during sweep phase.
//#define DUMP_MARKED

// Dump sweeped objects during sweep phase.
//#define DUMP_SWEEPED

// Dump global and GC stack pointers during mark phase.
// Especially useful if 'global_pointers' is incomplete.
//#define GC_DIAGNOSTICS

// Print 'C' for each compressed cons.
//#define VERBOSE_COMPRESSED_CONS

// Print names to built-in special forms FN and VAR.
//#define VERBOSE_DEFINES

// Print expressions before evaluation.
//#define VERBOSE_EVAL

// Print message if garbage collector takes action.
//#define VERBOSE_GC

// Print LOADed pathnames before evaluation.
//#define VERBOSE_LOAD

// Print READ expressions in REPL.
//#define VERBOSE_READ


/// Testing and debugging

// Sanity-check objects pointed to.
// If TARGET_UNIX, walk over heap to check if addresses
// are valid.
//#define CHECK_OBJ_POINTERS

// Give inappropriately happy developers a hard time.
// (Pre)releases require testing with this option set.
// Implies CHECK_OBJ_POINTERS
//#define GC_STRESS

// Aoid inlining of functions.
// * Allows setting breakpoints.
// * Reduces code size for releases on small machines.
//#define SLOW

// Enable extra checks that'll probably not kick in.
//#define PARANOID

// Do boundary checks of tag and GC stack pointers before
// moving them.
//#define GCSTACK_OVERFLOW_CHECKS
//#define GCSTACK_UNDERFLOW_CHECKS
//#define TAGSTACK_OVERFLOW_CHECKS
//#define TAGSTACK_UNDERFLOW_CHECKS


/// Release

// Disable C compiler level debugging.
//#define NDEBUG

// Disable all error handling.
//#define NAIVE


/// Disabling features

// No support for saving and loading images.
//#define NO_IMAGES

// Do not expand macros in REPL.
// Required to load all of the environment.
//#define NO_MACROEXPAND

// Disable calling user function ONERROR on errors.
//#define NO_ONERROR

// Do not print anonymous symbols.
//#define NO_PRINT_ANONYMOUS


/// Additional features

// Compressed conses.
//#define COMPRESSED_CONS

// Multiple heaps.
//#define FRAGMENTED_HEAP

// Real object 'nil' at address 0 to get around addiitonal
// pointer checks.  PLANNED!
//#define NULLOBJ

// Print 'x instead of (quote x).
//#define PRINT_SHORT_QUOTES

// GC sweep: do not copy if object didn't move.
// Adds extra code.
//#define SKIPPING_SWEEP


/// Memory allocation

// Stack and table sizes.
//#define STACK_SIZE          768
//#define TAGSTACK_SIZE       512
//#define RELOC_TABLE_ENTRIES 64

// Use malloc() to allocate the heap.
//#define MALLOCD_HEAP

// Use malloc() to allocate the tag stack.
// Will be taken from the heap otherwise.
//#define MALLOCD_TAGSTACK

// Use malloc() to allocate the object stack.
//#define MALLOCD_STACK

// Out-of-heap margin for calling ONERROR handlers.
#define ONETIME_HEAP_MARGIN (16 * sizeof (lispptr))


/// Target configurations

// Commodore C128
#ifdef TARGET_C128
#ifndef SLOW
    #define SLOW
#endif
#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#define STACK_SIZE          768
#define TAGSTACK_SIZE       512
#define RELOC_TABLE_ENTRIES 256
#define SKIPPING_SWEEP
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  (255 - sizeof (symbol))
#endif

// Commodore C16
#ifdef TARGET_C16
#ifndef NO_DEBUGGER
    #define NO_DEBUGGER
#endif
#ifndef NO_ONERROR
    #define NO_ONERROR
#endif
#ifndef SLOW
    #define SLOW
#endif
#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#define STACK_SIZE          768
#define TAGSTACK_SIZE       512
#define RELOC_TABLE_ENTRIES 64
#define MAX_SYMBOL  (255 - sizeof (symbol))
#endif

// Commodore C64
#ifdef TARGET_C64
#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#define STACK_SIZE          768
#define TAGSTACK_SIZE       512
#define RELOC_TABLE_ENTRIES 256
#define SKIPPING_SWEEP
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  (255 - sizeof (symbol))
#endif

// CP/M
#ifdef TARGET_CPM
#define MALLOCD_HEAP
#define HEAP_SIZE   16384
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#define STACK_SIZE          768
#define TAGSTACK_SIZE       512
#define RELOC_TABLE_ENTRIES 256
#define SKIPPING_SWEEP
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  (255 - sizeof (symbol))
#endif

// Commodore PET
#ifdef TARGET_PET
#ifndef SLOW
    #define SLOW
#endif
#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#define STACK_SIZE          768
#define TAGSTACK_SIZE       512
#define RELOC_TABLE_ENTRIES 128
#define SKIPPING_SWEEP
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  (255 - sizeof (symbol))
#endif

// Commodore Plus/4
#ifdef TARGET_PLUS4
#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#define STACK_SIZE          768
#define TAGSTACK_SIZE       512
#define RELOC_TABLE_ENTRIES 256
#define SKIPPING_SWEEP
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  (255 - sizeof (symbol))
#endif

// cc65's sim6502
#ifdef TARGET_SIM6502
#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#define STACK_SIZE          768
#define TAGSTACK_SIZE       512
#define RELOC_TABLE_ENTRIES 256
#define SKIPPING_SWEEP
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  (255 - sizeof (symbol))
#endif

// Commodore VIC-20/VC-20
#ifdef TARGET_VIC20
#ifndef SLOW
    #define SLOW
#endif
#define MALLOCD_HEAP
#define FRAGMENTED_HEAP
#define STACK_START         0x0400
#define STACK_END           0x0800
#define TAGSTACK_START      0x0800
#define TAGSTACK_END        0x1000
#define RELOC_TABLE_ENTRIES 256
#define SKIPPING_SWEEP
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  (255 - sizeof (symbol))
#endif

// Unixoids
#ifdef TARGET_UNIX
#ifndef SLOW
    #define SLOW
#endif
#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#define HEAP_SIZE           (64 * 1024U)
#define STACK_SIZE          HEAP_SIZE
#define TAGSTACK_SIZE       HEAP_SIZE
#define RELOC_TABLE_ENTRIES 256
#define SKIPPING_SWEEP
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  65536
#endif

// Sinclair ZX Spectrum
#ifdef TARGET_ZX
#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#define STACK_SIZE          768
#define TAGSTACK_SIZE       512
#define RELOC_TABLE_ENTRIES 256
#define SKIPPING_SWEEP
#define PRINT_SHORT_QUOTES
#endif

#if !defined (MALLOCD_HEAP) && !defined (HEAP_SIZE)
    #error "Either HEAP_SIZE or MALLOCD_HEAP must be defined."
#endif

#if defined (NDEBUG) && defined (DUMP_SWEEPED)
    #error "NDEBUG and DUMP_SWEEPED cannot be used together."
#endif

#ifdef NAIVE
    #ifndef NO_DEBUGGER
        #define NO_DEBUGGER
    #endif
    #ifndef NO_ONERROR
        #define NO_ONERROR
    #endif
    #ifndef NDEBUG
        #define NDEBUG
    #endif
    #ifdef PARANOID
        #error "NAIVE and PARANOID don't get along."
    #endif
#endif // #ifdef NAIVE

#ifdef __CC65__
    #define FASTCALL        __fastcall__
    #define HOST_DEBUGGER()
#else
    #define FASTCALL
    #define HOST_DEBUGGER() raise (SIGTRAP);
#endif

#ifndef NO_DEBUGGER
    #define PUSH_HIGHLIGHTED(x) \
        highlighted = x; \
        PUSH(highlighted);
    #define POP_HIGHLIGHTED() \
        POP(highlighted);
#else
    #define PUSH_HIGHLIGHTED(x)
    #define POP_HIGHLIGHTED()
#endif

#if defined(DUMP_MARKED) || defined(DUMP_SWEEPED)
    #define DUMP_LISPPTR
#endif

#if defined(GC_STRESS) && !defined(CHECK_OBJ_POINTERS)
    #define CHECK_OBJ_POINTERS
#endif

#if defined(COMPRESSED_CONS) && !defined(GC_AFTER_LOAD_THRESHOLD)
    #define GC_AFTER_LOAD_THRESHOLD 2048
#endif

#if defined(VERBOSE_COMPRESSED_CONS) && !defined(COMPRESSED_CONS)
    #error "VERBOSE_COMPRESSED_CONS has no effect without COMPRESSED_CONS."
#endif

typedef unsigned char  uchar;
typedef long           lispnum_t;
typedef void *         lispptr;
#ifdef __CC65__
typedef uchar          array_index_t;
typedef uchar          lispobj_size_t;
#else
typedef unsigned int   array_index_t;
typedef size_t         lispobj_size_t;
#endif

// TODO: Typedef for objects' type byte.

typedef struct _cons {
    uchar    type;
    lispptr  car;
    lispptr  cdr;
} cons;

#ifdef COMPRESSED_CONS
typedef struct _ccons {
    uchar    type;
    lispptr  car;
} ccons;
#endif

typedef struct _number {
    uchar      type;
    lispnum_t  value;
} number;

typedef struct _symbol {
    uchar           type;
    lispptr         value;
    lispptr         next;
    lispobj_size_t  length;
} symbol;

typedef struct _xlat_item {
    size_t   size;
    lispptr  pos;
} xlat_item;

typedef lispptr (*builtin_fun) (void);

struct builtin {
    const char * name;
    const char * argdef;
    builtin_fun  func;
};

struct heap_fragment {
    char * start;
    char * free;
    char * end;
};

typedef struct _image_header {
    char git_version[16];
    lispptr heap_start;
} image_header;

// ILOAD restart.
extern jmp_buf   restart_point;
extern jmp_buf * hard_repl_break;

extern lispptr * global_pointers[];

#ifdef FRAGMENTED_HEAP
extern struct  heap_fragment * heap;
extern struct  heap_fragment heaps[];
#endif

extern size_t  onetime_heap_margin;

extern lispptr universe;
extern char *  stack_start;
extern char    buffer[MAX_SYMBOL + 1];
extern lispptr current_expr;
extern lispptr current_function;
extern lispptr current_toplevel;
extern lispptr unexpanded_toplevel;
extern char *  last_errstr;
extern bool    debug_mode;
extern lispptr first_symbol;
extern lispptr last_symbol;
extern lispptr highlighted;
extern bool    do_highlight;
extern lispptr highlighted;
extern lispptr onerror_sym;
extern lispptr breakpoints_sym;

#ifndef NAIVE
extern char    error_code;
extern lispptr error_info;
#endif

extern lispptr t;
extern lispptr quote;
extern lispptr quasiquote;
extern lispptr unquote;
extern lispptr unquote_spliced;

extern char num_repls;
extern char num_debugger_repls;
extern bool do_break_repl;
extern bool do_continue_repl;
extern bool do_exit_program;

#ifdef COMPRESSED_CONS
extern bool do_compress_cons;
#endif

extern char *      heap_start;
extern char *      stack_end;
extern char *      tagstack_end;
extern xlat_item * xlat_end;

extern lispptr  lisp_fnin;
extern lispptr  lisp_fnout;

extern long bekloppies_start;
#ifdef TARGET_UNIX
extern long bekloppies (void);
#endif

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif

extern lispobj_size_t lisp_len;

// For processing lists with no recursions.
extern lispptr list_start;
extern lispptr list_last;

extern lispptr tmp;
extern lispptr tmp2;
extern char    tmpc;
extern char *  tmpstr;

extern char *      heap_free;
extern char *      heap_end;
extern char *      stack;
extern char *      stack_start;
extern char *      tagstack_start;
extern char *      tagstack;
extern xlat_item * xlat_start;

extern lispptr x;
extern lispptr args;
extern lispptr argdefs;
extern lispptr unevaluated_arg1;
extern lispptr arg1;
extern lispptr arg2;
extern lispptr arg2c;
extern lispptr value;
extern bool    unevaluated; // Tell eval0() to not evaluate arguments.

extern lispptr block_sym;
extern lispptr return_sym;
extern lispptr return_name;
extern lispptr return_value;

extern lispptr go_sym;
extern lispptr go_tag;

extern lispptr delayed_eval;

#ifndef NO_DEBUGGER
extern lispptr debug_step;
extern lispptr debugger_return_value_sym;
#endif

#ifndef NO_MACROEXPAND
extern bool    is_macroexpansion;
extern lispptr macroexpand_sym;
#endif

extern lispptr va; // Temporary in 'eval.c'.

#ifdef __CC65__
#pragma zpsym ("lisp_len")
#pragma zpsym ("tmp")
#pragma zpsym ("tmp2")
#pragma zpsym ("tmpc")
#pragma zpsym ("tmpstr")
#pragma zpsym ("heap_free")
#pragma zpsym ("heap_end")
#pragma zpsym ("stack")
#pragma zpsym ("stack_start")
#pragma zpsym ("tagstack_start")
#pragma zpsym ("tagstack")
#pragma zpsym ("xlat_start")
#ifndef NO_DEBUGGER
#pragma zpsym ("debug_step")
#endif
#pragma zpsym ("unevaluated")
#pragma zpsym ("x")
#pragma zpsym ("args")
#pragma zpsym ("argdefs")
#pragma zpsym ("unevaluated_arg1")
#pragma zpsym ("arg1")
#pragma zpsym ("arg2")
#pragma zpsym ("arg2c")
#pragma zpsym ("value")
#pragma zpsym ("block_sym")
#pragma zpsym ("return_sym")
#pragma zpsym ("return_value")
#pragma zpsym ("return_name")
#pragma zpsym ("go_sym")
#pragma zpsym ("go_tag")
#pragma zpsym ("delayed_eval")
#pragma zpsym ("list_start")
#pragma zpsym ("list_last")
#pragma zpsym ("va")
#pragma bss-name (pop)
#endif

// NOT_NIL() doesn't work.  Either the onset of dementia
// or terrible bugs hiding.
#define nil     ((lispptr) 0)
#ifdef __CC65__
    #define NOT(x)      !((size_t) x & 0xff00)
    // TOOD: Fix https://github.com/cc65/cc65/issues/2487
    //#define NOT_NIL(x)  ((size_t) x & 0xff00)
    #define NOT_NIL(x)  (x)
#else
    #define NOT(x)      (!x)
    #define NOT_NIL(x)  (x)
#endif

#ifdef GC_STRESS
extern bool do_gc_stress;
#endif

#define _GCSTACK_CHECK_OVERFLOW() \
    if (stack == stack_start) \
        stack_overflow ()
#define _GCSTACK_CHECK_UNDERFLOW() \
    if (stack == stack_end) \
        stack_underflow ()
#define _TAGSTACK_CHECK_OVERFLOW() \
    if (tagstack == tagstack_start) \
        tagstack_overflow ()
#define _TAGSTACK_CHECK_UNDERFLOW() \
    if (tagstack == tagstack_end) \
        tagstack_underflow ()

#ifdef GCSTACK_OVERFLOW_CHECKS
    #define GCSTACK_CHECK_OVERFLOW() _GCSTACK_CHECK_OVERFLOW()
#else
    #define GCSTACK_CHECK_OVERFLOW()
#endif
#ifdef GCSTACK_UNDERFLOW_CHECKS
    #define GCSTACK_CHECK_UNDERFLOW() _GCSTACK_CHECK_UNDERFLOW()
#else
    #define GCSTACK_CHECK_UNDERFLOW()
#endif

#ifdef TAGSTACK_OVERFLOW_CHECKS
    #define TAGSTACK_CHECK_OVERFLOW() _TAGSTACK_CHECK_OVERFLOW()
#else
    #define TAGSTACK_CHECK_OVERFLOW()
#endif
#ifdef TAGSTACK_OVERFLOW_CHECKS
    #define TAGSTACK_CHECK_UNDERFLOW() _TAGSTACK_CHECK_UNDERFLOW()
#else
    #define TAGSTACK_CHECK_UNDERFLOW()
#endif

#ifdef SLOW
    #define PUSH(x)      pushgc (x)
    #define POP(x)       do { x = popgc (); } while (0)
    #define PUSH_TAG(x)  pushtag (x)
    #define POP_TAG(x)   do { x = poptag (); } while (0)
    #define PUSH_TAGW(x) pushtagw (x)
    #define POP_TAGW(x)  do { x = poptagw (); } while (0)
#else // #ifdef SLOW
    #define PUSH(x) \
        do { \
            GCSTACK_CHECK_OVERFLOW(); \
            stack -= sizeof (lispptr); \
            *(lispptr *) stack = x; \
        } while (0)
    #define POP(x) \
        do { \
            GCSTACK_CHECK_UNDERFLOW(); \
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

// TODO: Rename from TAG_* to EVAL0_* to make
// their function more obvious outside eval0().
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
#define TYPE_CCONS      (TYPE_CONS | TYPE_EXTENDED)

#define TYPE(x)         (*((char *) (x))) // TODO: Rename.
#define TYPEBITS(x)     (TYPE(x) & TYPE_MASK)

#define MARKED(x)       (NOT(x) || TYPE(x) & TYPE_MARKED)
#define MARK(x)         (TYPE(x) |= TYPE_MARKED)
#define UNMARK(x)       (TYPE(x) &= ~TYPE_MARKED)

#define CONS(x)         ((cons *) (x))

#ifdef COMPRESSED_CONS
    // CDR of a compressed cons is the address of the next
    // object (which is also a cons).
    #define CCONS_CDR(x)    (&CONS(x)->cdr)
#endif

#define _ATOM(x)        (NOT(x) || !(TYPE(x) & TYPE_CONS))
#define _CONSP(x)       (NOT_NIL(x) && (TYPE(x) & TYPE_CONS))
#define _SYMBOLP(x)     (NOT(x) || (TYPE(x) & TYPE_SYMBOL))
#define _BUILTINP(x)    (NOT_NIL(x) && (TYPE(x) & TYPE_BUILTIN))
#define _NUMBERP(x)     (NOT_NIL(x) && (TYPE(x) & TYPE_NUMBER))
#define _LISTP(x)       (NOT(x) || (TYPE(x) & TYPE_CONS))
#define _SPECIALP(x)    (NOT_NIL(x) && (TYPE(x) & TYPE_SPECIAL) == TYPE_SPECIAL)
#define _NAMEDP(x)      (NOT_NIL(x) && TYPE(x) & (TYPE_SYMBOL | TYPE_BUILTIN))
#define _EXTENDEDP(x)   (TYPE(x) & TYPE_EXTENDED)

#define EXTENDEDP(x)    (NOT_NIL(x) && (TYPE(x) & TYPE_EXTENDED))

#define LIST_CAR(x)     (NOT(x) ? x : CAR(x))
#define LIST_CDR(x)     (NOT(x) ? x : CDR(x))

#define _SETCAR(x, v) (CONS(x)->car = v)
#ifdef NAIVE
    #define _SETCDR_CHECK(x)
#else
    #define _SETCDR_CHECK(x) \
        if (_EXTENDEDP(x)) \
            error_set_ccons_cdr ();
#endif
#ifdef COMPRESSED_CONS
    #define _SETCDR(x, v) \
        do { \
            _SETCDR_CHECK(x); \
            CONS(x)->cdr = v; \
        } while (0);
#else
    #define _SETCDR(x, v) \
        (CONS(x)->cdr = v)
#endif // #ifdef COMPRESSED_CONS

#ifdef SLOW
    #define CAR(x)       (lisp_car (x))
    #define CDR(x)       (lisp_cdr (x))
    #define SETCAR(x, v) (lisp_setcar (x, v))
    #define SETCDR(x, v) (lisp_setcdr (x, v))
    #define ATOM(x)      (lisp_atom (x))
    #define CONSP(x)     (lisp_consp (x))
    #define LISTP(x)     (lisp_listp (x))
    #define NUMBERP(x)   (lisp_numberp (x))
    #define SYMBOLP(x)   (lisp_symbolp (x))
    #define BUILTINP(x)  (lisp_builtinp (x))
    #define SPECIALP(x)  (lisp_specialp (x))
#else // #ifdef SLOW
    #define CAR(x)       (CONS(x)->car)
        #ifdef COMPRESSED_CONS
            #define CDR(x)   (_EXTENDEDP(x) ? CCONS_CDR(x) : CONS(x)->cdr)
        #else
            #define CDR(x)   (CONS(x)->cdr)
    #endif
    #define SETCAR(x, v) _SETCAR(x, v)
    #define SETCDR(x, v) _SETCDR(x, v)
    #define ATOM(x)      _ATOM(x)
    #define CONSP(x)     _CONSP(x)
    #define LISTP(x)     _LISTP(x)
    #define NUMBERP(x)   _NUMBERP(x)
    #define SYMBOLP(x)   _SYMBOLP(x)
    #define BUILTINP(x)  _BUILTINP(x)
    #define SPECIALP(x)  _SPECIALP(x)
#endif // #ifdef SLOW

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

#define ERROR_TYPE              1
#define ERROR_ARG_MISSING       2
#define ERROR_TAG_MISSING       3
#define ERROR_TOO_MANY_ARGS     4
#define ERROR_NOT_FUNCTION      5
#define ERROR_ARGNAME_TYPE      6
#define ERROR_OUT_OF_HEAP       7
#define ERROR_NO_PAREN          8
#define ERROR_STALE_PAREN       9
#define ERROR_SYM_TOO_LONG      10
#define ERROR_QUOTE_MISSING     11
#define ERROR_FILEMODE          12
#define ERROR_USER              13

// Returned to OS on exit after internal error.
#define ERROR_INTERNAL      12

#if !defined (NDEBUG) && (defined(GC_STRESS) || defined(CHECK_OBJ_POINTERS))
    #define CHKPTR(x)   check_lispptr (x)
#else
    #define CHKPTR(x)
#endif

extern void     FASTCALL expand_universe (lispptr);
extern lispptr  FASTCALL make_cons       (lispptr, lispptr);
extern lispptr  FASTCALL make_number     (lispnum_t);
extern lispptr  FASTCALL alloc_symbol    (char *, uchar len);
extern lispptr  FASTCALL make_symbol     (char *, uchar len);
extern lispptr           read_expr       (void);
extern lispptr           read_symbol     (void);
extern lispptr           read_number     (void);
extern lispptr  FASTCALL print           (lispptr);
extern lispptr  FASTCALL dprint          (lispptr);
extern void     FASTCALL set_channels    (simpleio_chn_t in, simpleio_chn_t out);
extern lispptr           bi_setin        (void);
extern lispptr           bi_setout       (void);

// Arguments in global 'x'.
extern lispptr           eval0           (void);
extern lispptr           eval            (void);
extern lispptr           eval_list       (void);
extern lispptr           funcall         (void);

extern void              gc               (void);
extern lispobj_size_t    FASTCALL objsize (char *);
extern size_t            heap_free_size   (void);

#define REPL_STD        0
#define REPL_DEBUGGER   1
#define REPL_LOAD       2
extern lispptr  FASTCALL lisp_repl    (char mode);
extern bool     FASTCALL load         (char * pathname);
extern bool     FASTCALL image_load   (char * pathname);
extern bool     FASTCALL image_save   (char * pathname);

extern void     FASTCALL add_builtins  (const struct builtin *);
extern lispptr           debugger      (void);

#define COPY_LIST       0
#define COPY_BUTLAST    1
#define COPY_REMOVE     2
extern int      FASTCALL length              (lispptr);
extern lispptr  FASTCALL copy_list           (lispptr, char mode, lispptr excluded);
extern lispptr  FASTCALL butlast             (lispptr);
extern lispptr  FASTCALL last                (lispptr);
extern lispptr  FASTCALL member              (lispptr needle, lispptr haystack);

extern void     FASTCALL pushgc         (lispptr);
extern lispptr           popgc          (void);
extern void     FASTCALL pushtag        (char);
extern char              poptag         (void);
extern void     FASTCALL pushtagw       (lispptr);
extern lispptr           poptagw        (void);
extern lispptr  FASTCALL lisp_car       (lispptr);
extern lispptr  FASTCALL lisp_cdr       (lispptr);
extern void     FASTCALL lisp_setcar    (lispptr x, lispptr v);
extern void     FASTCALL lisp_setcdr    (lispptr x, lispptr v);
extern bool     FASTCALL lisp_atom      (lispptr);
extern bool     FASTCALL lisp_consp     (lispptr);
extern bool     FASTCALL lisp_listp     (lispptr);
extern bool     FASTCALL lisp_numberp   (lispptr);
extern bool     FASTCALL lisp_symbolp   (lispptr);
extern bool     FASTCALL lisp_builtinp  (lispptr);
extern bool     FASTCALL lisp_specialp  (lispptr);

#ifndef NAIVE
extern void     FASTCALL internal_error      (char * msg);
extern void     FASTCALL internal_error_ptr  (void *, char * msg);
extern void     FASTCALL error               (char code, char * msg);
extern void     FASTCALL error_argname       (lispptr);
extern lispptr  FASTCALL error_cons_expected (lispptr);
extern void     FASTCALL err_type            (char * type, lispptr x, char errorcode);
extern void              stack_overflow      (void);
extern void              stack_underflow     (void);
extern void              tagstack_overflow   (void);
extern void              tagstack_underflow  (void);
#ifdef COMPRESSED_CONS
extern void              error_set_ccons_cdr (void);
#endif // #ifdef COMPRESSED_CONS

extern void     FASTCALL bi_tcheck           (lispptr, uchar type, char errorcode);
extern void     FASTCALL check_stacks        (char * old_stack, char * old_tagstack);
extern void              print_error_info    (void);
#ifdef CHECK_OBJ_POINTERS
extern void              check_lispptr       (char *);
#endif // #ifdef CHECK_OBJ_POINTERS

extern char *   FASTCALL typestr             (lispptr *);
#ifdef TARGET_UNIX
extern void              dump_lispptr        (char *);
extern void              dump_heap           (void);
#endif // #ifdef TARGET_UNIX
#endif // #ifndef NAIVE

extern void     FASTCALL name_to_buffer      (lispptr s);
extern void     FASTCALL make_call           (lispptr args);
extern void     FASTCALL make_car_call       (void);

// Switch to 'heap'.
extern void             switch_heap          (void);
size_t                  heap_free_size       (void);

extern void init_builtins (void);
extern void init_eval     (void);
extern bool init_heap     (void);
extern void init_list     (void);
extern void init_onerror  (void);
extern void init_repl     (void);
extern void heap_add_init_areas (void);

#endif // #ifndef __LIBLISP_H__
