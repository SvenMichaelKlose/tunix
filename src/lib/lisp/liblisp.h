#ifndef __LIBLISP_H__
#define __LIBLISP_H__

////////////////////////////
/// Compile-time options ///
////////////////////////////
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

// Print expressions before evaluation.
//#define VERBOSE_EVAL

// Print message if garbage collector takes action.
//#define VERBOSE_GC

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

// Call HOST_DEBUGGER() in error().
//#define HOST_DEBUGGER_ON_ERROR

// Aoid inlining of functions.
// * Allows setting breakpoints.
// * Reduces code size for releases on small machines.
//#define SLOW

// Load "test-all.lsp" at end of boot.
//#define TEST_ALL

// Load environment tests.
//#define TEST_ENVIRONMENT

// Call test() before start-up.
//#define TEST_INTERPRETER

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

// Disable loading DOTEXPAND on boot.
//#define NO_DOTEXPAND

// No support for saving and loading images.
//#define NO_IMAGE

// No highlighted expressions in debug info.
//#define NO_HIGHLIGHTING.

// Do not call MACROEXPAND in REPL.
//#define NO_MACROEXPAND

// Disable calling user function ONERROR on errors.
//#define NO_ONERROR

// Do not print anonymous symbols.
//#define NO_PRINT_ANONYMOUS

// Do not load QUASIQUOTE.
//#define NO_QUASIQUOTE

// Disable built-ins.
//#define NO_BUILTIN_APPEND // Native is smaller.
//#define NO_BUILTIN_ASSOC
//#define NO_BUILTIN_CHAR_AT
//#define NO_BUILTIN_GC
//#define NO_BUILTIN_FREE
//#define NO_BUILTIN_LOAD
//#define NO_BUILTIN_NCONC
//#define NO_BUILTIN_NTHCDR
//#define NO_BUILTIN_PRINT
//#define NO_BUILTIN_READ
//#define NO_BUILTIN_READ_LINE
//#define NO_BUILTIN_SUBSEQ
//#define NO_BUILTIN_TIME

#ifdef NO_BUILTIN_NTHCDR
    #define NO_BUILTIN_SUBSEQ
#endif

// Disable groups of built-ins.
//#define NO_BUILTIN_GROUP_ARITH
//#define NO_BUILTIN_GROUP_BITOPS
//#define NO_BUILTIN_GROUP_DEFINITIONS
//#define NO_BUILTIN_GROUP_DIRECTORY
//#define NO_BUILTIN_GROUP_FILE
//#define NO_BUILTIN_GROUP_IMAGE
//#define NO_BUILTIN_GROUP_RAW_ACCESS
//#define NO_BUILTIN_GROUP_SYMBOL_NAME

// Print names to built-in special forms FN and VAR.
//#define NO_VERBOSE_DEFINES

// Print LOADed pathnames before evaluation.
//#define NO_VERBOSE_LOAD

// Don't use zeropage locations with cc65.
//#define NO_ZEROPAGE


/// Additional features

// Compressed conses.
//#define COMPRESSED_CONS

// Fast 8-bit NIL tests.
//#define FAST_NIL

// Restart GC if relocation table is full
#define RESTART_GC_ON_FULL_RELOC

// Unix sockets (incl. built-ins)
//#define HAVE_SOCKETS

// Real NIL in memory.
//#define REAL_NIL

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

// Use zeropage locations with cc65.
//#define USE_ZEROPAGE


/// Memory allocation

// Stack and table sizes.
#define STACK_SIZE          2560
#define TAGSTACK_SIZE       512
#define RELOC_TABLE_ENTRIES 256

// Use malloc() to allocate the heap.
//#define MALLOCD_HEAP

// Use malloc() to allocate the tag stack.
// Will be taken from the heap otherwise.
//#define MALLOCD_TAGSTACK

// Use malloc() to allocate the object stack.
//#define MALLOCD_STACK

// Out-of-heap margin for calling ONERROR handlers.
#define ONETIME_HEAP_MARGIN (16 * sizeof (lispptr))


////////////////////////////
/// Target configuration ///
////////////////////////////

// Apple II
#ifdef TARGET_APPLE2
#define FAST_NIL
#define MALLOCD_HEAP
#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#undef RELOC_TABLE_ENTRIES
#define RELOC_TABLE_ENTRIES 256
#define SKIPPING_SWEEP
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  (255 - sizeof (symbol))
#ifndef NO_BUILTIN_TIME
    #define NO_BUILTIN_TIME
#endif
#ifndef NO_BUILTIN_GROUP_DIRECTORY
    #define NO_BUILTIN_GROUP_DIRECTORY
#endif
#endif

// Apple II enhanced
#ifdef TARGET_APPLE2ENH
#define FAST_NIL
#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#undef RELOC_TABLE_ENTRIES
#define RELOC_TABLE_ENTRIES 256
#define SKIPPING_SWEEP
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  (255 - sizeof (symbol))
#ifndef NO_BUILTIN_TIME
    #define NO_BUILTIN_TIME
#endif
#ifndef NO_BUILTIN_GROUP_DIRECTORY
    #define NO_BUILTIN_GROUP_DIRECTORY
#endif
#endif

// Atari XL
#ifdef TARGET_ATARIXL
#define FAST_NIL
#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#undef RELOC_TABLE_ENTRIES
#define RELOC_TABLE_ENTRIES 256
#define SKIPPING_SWEEP
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  (255 - sizeof (symbol))
#ifndef NO_BUILTIN_TIME
    #define NO_BUILTIN_TIME
#endif
#ifndef NO_BUILTIN_GROUP_DIRECTORY
    #define NO_BUILTIN_GROUP_DIRECTORY
#endif
#endif

// Commodore C128
#ifdef TARGET_C128
#define FAST_NIL
#ifndef SLOW
    #define SLOW
#endif
#define NO_IMAGE
#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#undef RELOC_TABLE_ENTRIES
#define RELOC_TABLE_ENTRIES 128
#define SKIPPING_SWEEP
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  (255 - sizeof (symbol))
#endif

// Commodore C16
#ifdef TARGET_C16
#define FAST_NIL
#define MINIMALISTIC
#ifndef NO_ONERROR
    #define NO_ONERROR
#endif
#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#undef STACK_SIZE
#define STACK_SIZE          512
#undef TAGSTACK_SIZE
#define TAGSTACK_SIZE       256
#undef RELOC_TABLE_ENTRIES
#define RELOC_TABLE_ENTRIES 64
#define MAX_SYMBOL  (255 - sizeof (symbol))
#endif

// Commodore C64
#ifdef TARGET_C64
#define FAST_NIL
#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#define SKIPPING_SWEEP
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  (255 - sizeof (symbol))
#endif

// CP/M
#ifdef TARGET_CPM
#define FAST_NIL
#define MALLOCD_HEAP
#define HEAP_SIZE           8192
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#define STACK_SIZE          1280
#define TAGSTACK_SIZE       384
#define RELOC_TABLE_ENTRIES 256
#define SKIPPING_SWEEP
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  (255 - sizeof (symbol))
#ifndef NO_BUILTIN_TIME
    #define NO_BUILTIN_TIME
#endif
#ifndef NO_BUILTIN_GROUP_DIRECTORY
    #define NO_BUILTIN_GROUP_DIRECTORY
#endif
#endif

// Commodore PET
#ifdef TARGET_PET
#ifndef SLOW
    #define SLOW
#endif
#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  (255 - sizeof (symbol))
#endif

// Commodore Plus/4
#ifdef TARGET_PLUS4
#define FAST_NIL
#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#define SKIPPING_SWEEP
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  (255 - sizeof (symbol))
#endif

// cc65's sim6502
#ifdef TARGET_SIM6502
#define FAST_NIL
#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#define SKIPPING_SWEEP
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  (255 - sizeof (symbol))
#ifndef NO_BUILTIN_TIME
    #define NO_BUILTIN_TIME
#endif
#ifndef NO_BUILTIN_GROUP_DIRECTORY
    #define NO_BUILTIN_GROUP_DIRECTORY
#endif
#endif

// Unixoids
#ifdef TARGET_UNIX
#if !defined(SLOW) && defined(NDEBUG)
    #define SLOW
#endif
#define HAVE_SOCKETS
#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#undef HEAP_SIZE
#define HEAP_SIZE           (256 * 1024U)
#undef STACK_SIZE
#define STACK_SIZE          (HEAP_SIZE / 8)
#undef TAGSTACK_SIZE
#define TAGSTACK_SIZE       (HEAP_SIZE / 8)
#undef RELOC_TABLE_ENTRIES
#define RELOC_TABLE_ENTRIES 512
#define SKIPPING_SWEEP
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  65536
#define NO_BUILTIN_DIRECTORY

#if defined(REAL_NIL) && defined(FAST_NIL)
    #error "REAL_NIL and FAST_NIL cannot be used together with TARGET_UNIX"
#endif
#if defined(REAL_NIL) && !defined(NIL_NOT_0)
    #define NIL_NOT_0
#endif
#endif

// Commodore VIC-20/VC-20
#ifdef TARGET_VIC20
#define FAST_NIL
#define MINIMALISTIC
#define MALLOCD_HEAP
#define FRAGMENTED_HEAP
#define STACK_START         0x0400
#define STACK_END           0x0800
#define TAGSTACK_START      0x0800
#define TAGSTACK_END        0x0900
#undef RELOC_TABLE_ENTRIES
#define RELOC_TABLE_ENTRIES 128
#define PRINT_SHORT_QUOTES
#define MAX_SYMBOL  (255 - sizeof (symbol))
#endif

// Sinclair ZX Spectrum
#ifdef TARGET_ZX
#define FAST_NIL
#define MALLOCD_HEAP
#define MALLOCD_STACK
#define MALLOCD_TAGSTACK
#define SKIPPING_SWEEP
#define PRINT_SHORT_QUOTES
#ifndef NO_BUILTIN_TIME
    #define NO_BUILTIN_TIME
#endif
#ifndef NO_BUILTIN_GROUP_DIRECTORY
    #define NO_BUILTIN_GROUP_DIRECTORY
#endif
#endif

/////////////////////////////
/// Generic configuration ///
/////////////////////////////

#ifdef MINIMALISTIC
#ifndef SLOW
    #define SLOW
#endif
#ifndef NO_DEBUGGER
    #define NO_DEBUGGER
#endif
#ifndef NO_DOTEXPAND
    #define NO_DOTEXPAND
#endif
#ifndef NO_IMAGE
    #define NO_IMAGE
#endif
#ifndef NO_BUILTIN_GROUP_DIRECTORY
    #define NO_BUILTIN_GROUP_DIRECTORY
#endif
#ifndef NO_BUILTIN_GROUP_BITOPS
    #define NO_BUILTIN_GROUP_BITOPS
#endif
#ifndef NO_BUILTIN_GROUP_IMAGE
    #define NO_BUILTIN_GROUP_IMAGE
#endif
#ifndef NO_BUILTIN_READ_LINE
    #define NO_BUILTIN_READ_LINE
#endif
#ifndef NO_BUILTIN_SUBSEQ
    #define NO_BUILTIN_SUBSEQ
#endif
#ifdef SKIPPING_SWEEP
    #undef SKIPPING_SWEEP
#endif
#endif // #ifdef MINIMALISTIC

#ifdef MICROSCOPIC
#ifndef NO_IMAGE
    #define NO_IMAGE
#endif
#ifndef NAIVE
    #define NAIVE
#endif
#ifndef SLOW
    #define SLOW
#endif
#ifndef NO_ONERROR
    #define NO_ONERROR
#endif
#ifndef NO_DEBUGGER
    #define NO_DEBUGGER
#endif
#ifndef NO_QUASIQUOTE
    #define NO_QUASIQUOTE
#endif
#ifndef NO_MACROEXPAND
    #define NO_MACROEXPAND
#endif
#ifndef NO_BUILTIN_GROUP_BITOPS
    #define NO_BUILTIN_GROUP_BITOPS
#endif
#ifndef NO_BUILTIN_GROUP_DIRECTORY
    #define NO_BUILTIN_GROUP_DIRECTORY
#endif
#ifndef NO_BUILTIN_GROUP_FILE
    #define NO_BUILTIN_GROUP_FILE
#endif
#ifndef NO_BUILTIN_ASSOC
    #define NO_BUILTIN_ASSOC
#endif
#ifndef NO_BUILTIN_CHAR_AT
    #define NO_BUILTIN_CHAR_AT
#endif
#ifndef NO_BUILTIN_RAW_ACCESS
    #define NO_BUILTIN_RAW_ACCESS
#endif
#ifndef NO_BUILTIN_GC
    #define NO_BUILTIN_GC
#endif
#ifndef NO_BUILTIN_LOAD
    #define NO_BUILTIN_LOAD
#endif
#ifndef NO_BUILTIN_PRINT
    #define NO_BUILTIN_PRINT
#endif
#ifndef NO_BUILTIN_READ
    #define NO_BUILTIN_READ
#endif
#ifndef NO_BUILTIN_READ_LINE
    #define NO_BUILTIN_READ_LINE
#endif
#ifndef NO_BUILTIN_GROUP_RAW_ACCESS
    #define NO_BUILTIN_GROUP_RAW_ACCESS
#endif
#ifndef NO_BUILTIN_GROUP_SYMBOL_NAME
    #define NO_BUILTIN_GROUP_SYMBOL_NAME
#endif
#endif // #ifdef MICROSCOPIC

#ifdef DEVELOPMENT
#ifndef SLOW
    #define SLOW
#endif
#ifndef CHECK_OBJ_POINTERS
    #define CHECK_OBJ_POINTERS
#endif
#endif // #ifdef DEVELOPMENT

//////////////////////////
/// CONFIG ADJUSTMENTS ///
//////////////////////////

#if !defined (MALLOCD_HEAP) && !defined (HEAP_SIZE)
    #error "Either HEAP_SIZE or MALLOCD_HEAP must be defined."
#endif

#if defined (NDEBUG) && defined (DUMP_SWEEPED)
    #error "NDEBUG and DUMP_SWEEPED cannot be used together."
#endif

#if defined(NIL_NOT_0) && !defined(REAL_NIL)
    #error "NIL_NOT_0 requires REAL_NIL"
#endif

#if defined(NO_IMAGE) && !defined(NO_BUILTIN_GROUP_IMAGE)
    #define NO_BUILTIN_GROUP_IMAGE
#endif

#ifdef NAIVE
    #ifndef NO_DEBUGGER
        #define NO_DEBUGGER
    #endif
    #ifndef NO_HIGHLIGHTING
        #define NO_HIGHLIGHTING
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

#if defined(SLOW) && defined(NOT_SLOW) && !defined(DEVELOPMENT)
    #undef SLOW
#endif

#if defined(__CC65__) && !defined(NO_ZEROPAGE)
    #define USE_ZEROPAGE
#endif

#ifdef __CC65__
    #define FASTCALL        __fastcall__
    #define HOST_DEBUGGER()
#else
    #define FASTCALL
    #define HOST_DEBUGGER() raise (SIGTRAP);
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

#ifndef NO_HIGHLIGHTING
    #define HIGHLIGHT(x)  highlighted = x
#else
    #define HIGHLIGHT(x)
#endif

//////////////
/// TYPES  ///
//////////////

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

///////////////
/// OBJECTS ///
///////////////

#ifdef NIL_NOT_0

struct real_nil {
    symbol  s;
    char    name[3];
};

#endif // #ifdef NIL_NOT_0

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
    char git_version[8];
    lispptr heap_start;
} image_header;

////////////////////////
/// GLOBAL VARIABLES ///
////////////////////////

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
#ifndef NDEBUG
extern bool    debug_mode;
#endif
extern lispptr first_symbol;
extern lispptr last_symbol;
#ifndef NO_DEBUGGER
extern lispptr breakpoints_sym;
#endif
#ifndef NO_ONERROR
extern lispptr onerror_sym;
extern lispptr fail_sym;
#endif

#if !defined(NO_VERBOSE_LOAD) && !defined(NO_VERBOSE_DEFINES)
extern lispptr vp_symbol;
#endif

extern lispptr dot_symbol;

#ifndef NO_HIGHLIGHTING
extern lispptr highlighted;
extern bool    do_highlight;
#endif

#ifndef NAIVE
extern char    error_code;
extern lispptr failed_obj;
extern lispptr error_info;
#endif

extern lispptr t;
extern lispptr quote;
#ifndef NO_QUASIQUOTE
extern lispptr quasiquote;
extern lispptr unquote;
extern lispptr unquote_spliced;
#endif

extern char num_repls;
extern char num_debugger_repls;

#define BRK_CONTINUE    1
#define BRK_RETURN      2
#define BRK_EXIT        3
extern char do_break_repl;

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

#ifdef USE_ZEROPAGE
#pragma bss-name (push, "ZEROPAGE")
#endif

// For processing lists with no recursions.
extern lispptr list_start;
extern lispptr list_last;

extern lispptr tmp;
extern lispptr tmp2;
extern char    tmpc;
extern char *  tmpstr;

#ifdef TARGET_CPM
extern long heap;  // For the 'zcc' compiler suite.
#endif

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
extern lispptr repl_value;
#endif

#ifndef NO_MACROEXPAND
extern bool    is_macroexpansion;
extern lispptr expand_sym;
#endif

extern lispptr make_cons_tmp;
extern lispptr make_cons_car;
extern lispptr make_cons_cdr;

#ifdef NIL_NOT_0
extern struct real_nil real_nil;
#endif

#ifdef USE_ZEROPAGE
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
#ifdef NIL_NOT_0
#pragma zpsym ("real_nil")
#endif
#pragma bss-name (pop)
#endif // #ifdef USE_ZEROPAGE

///////////
/// NIL ///
///////////

#ifdef NIL_NOT_0
#define nil     ((lispptr) &real_nil)
#else
#define nil     ((lispptr) 0)
#endif

#ifdef FAST_NIL
    #define NOT(x)      !((uintptr_t) x & (-1 & ~0xff))
    #define NOT_NIL(x)  ((uintptr_t) x & (-1 & ~0xff))
#else
    #define NOT(x)      (x == nil)
    #define NOT_NIL(x)  (x != nil)
#endif

#ifdef REAL_NIL
    #define PNOT(x)     false
    #define PNOT_NIL(x) true
#else
    #define PNOT(x)     NOT(x)
    #define PNOT_NIL(x) NOT_NIL(x)
#endif

#ifdef GC_STRESS
extern bool do_gc_stress;
#endif

////////////////////////////
/// STACK POINTER CHECKS ///
////////////////////////////

#define _GCSTACK_CHECK_OVERFLOW() \
    if (stack <= stack_start) \
        stack_overflow ()
#define _GCSTACK_CHECK_UNDERFLOW() \
    if (stack >= stack_end) \
        stack_underflow ()
#define _TAGSTACK_CHECK_OVERFLOW() \
    if (tagstack <= tagstack_start) \
        tagstack_overflow ()
#define _TAGSTACK_CHECK_UNDERFLOW() \
    if (tagstack >= tagstack_end) \
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

//////////////////////
/// STACK PUSH/POP ///
//////////////////////

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

/////////////////////////
/// eval0 RETURN TAGS ///
/////////////////////////

// TODO: Rename from TAG_* to EVAL0_* to make
// their function more obvious outside eval0().
#define TAG_DONE                  0
#define TAG_NEXT_BUILTIN_ARG      1
#define TAG_NEXT_ARG              2
#define TAG_NEXT_BODY_STATEMENT   3
#define TAG_NEXT_BLOCK_STATEMENT  4

//////////////////////
/// LIST ITERATION ///
//////////////////////

#define DOLIST(x, init) \
    for (x = init; CONSP(x); x = CDR(x))
#define TYPESAFE_DOLIST(x, init) \
    for (x = init; CONSP(x); x = LIST_CDR(x))

/////////////
/// CASTS ///
/////////////

#define BOOL(x)         ((x) ? t : nil)
#define CONS(x)         ((cons *) (x))
#define NUMBER(n)       ((number *) (n))
#define SYMBOL(s)       ((symbol *) (s))

///////////////////
/// OBJECT TYPE ///
///////////////////

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

#ifdef COMPRESSED_CONS
    // CDR of a compressed cons is the address of the next
    // object (which is also a cons).
    #define CCONS_CDR(x)    (&CONS(x)->cdr)
#endif

#define _ATOM(x)        (PNOT(x) || !(TYPE(x) & TYPE_CONS))
#define _CONSP(x)       (PNOT_NIL(x) && (TYPE(x) & TYPE_CONS))
#define _SYMBOLP(x)     (PNOT(x) || (TYPE(x) & TYPE_SYMBOL))
#define _BUILTINP(x)    (PNOT_NIL(x) && (TYPE(x) & TYPE_BUILTIN))
#define _NUMBERP(x)     (PNOT_NIL(x) && (TYPE(x) & TYPE_NUMBER))
#define _LISTP(x)       (NOT(x) || (TYPE(x) & TYPE_CONS))
#define _SPECIALP(x)    (PNOT_NIL(x) && (TYPE(x) & TYPE_SPECIAL) == TYPE_SPECIAL)
#define _NAMEDP(x)      (PNOT_NIL(x) && TYPE(x) & (TYPE_SYMBOL | TYPE_BUILTIN))
#define _EXTENDEDP(x)   (TYPE(x) & TYPE_EXTENDED)
#define EXTENDEDP(x)    (PNOT_NIL(x) && (TYPE(x) & TYPE_EXTENDED))

//////////////
/// CONSES ///
//////////////

#define LIST_CAR(x)     (NOT(x) ? x : CAR(x))
#define LIST_CDR(x)     (NOT(x) ? x : CDR(x))

#define _SETCAR(x, v) (CONS(x)->car = v)

#ifdef NAIVE
    #define _SETCDR_CHECK(x)
#else
    #define _SETCDR_CHECK(x) \
        if (_EXTENDEDP(x)) \
            error_set_ccons_cdr ();
#endif // #ifdef NAIVE

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

/////////////////////////
/// INLINING WRAPPERS ///
/////////////////////////

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

///////////////
/// NUMBERS ///
///////////////

#define NUMBER_VALUE(n)         (NUMBER(n)->value)
#define SET_NUMBER_VALUE(n, x)  (NUMBER(n)->value = x)

///////////////
/// SYMBOLS ///
///////////////

#define SYMBOL_NEXT(s)          (SYMBOL(s)->next)
#define SYMBOL_VALUE(s)         (SYMBOL(s)->value)
#define SYMBOL_LENGTH(s)        (SYMBOL(s)->length)
#define SYMBOL_NAME(s)          ((char *) s + sizeof (symbol))
#define SET_SYMBOL_NEXT(s, x)   (SYMBOL(s)->next = x)
#define SET_SYMBOL_VALUE(s, x)  (SYMBOL(s)->value = x)

////////////////////////////
/// FUNCTION EXPRESSIONS ///
////////////////////////////

#define FUNARGS(x)  CAR(x)
#define FUNBODY(x)  CDR(x)

//////////////
/// ERRORS ///
//////////////

#define ERROR_TYPE              1
#define ERROR_ARG_MISSING       2
#define ERROR_TAG_MISSING       3
#define ERROR_TOO_MANY_ARGS     4
#define ERROR_NOT_FUNCTION      5
#define ERROR_ARGNAME_TYPE      6
#define ERROR_NO_BLOCKNAME      7
#define ERROR_OUT_OF_HEAP       8
#define ERROR_PAREN_MISSING     9
#define ERROR_STALE_PAREN       10
#define ERROR_SYM_TOO_LONG      11
#define ERROR_QUOTE_MISSING     12
#define ERROR_LOST_RETURN       13
#define ERROR_LOST_GO           14
#define ERROR_NEGATIVE          15
#define ERROR_FILEMODE          16
#define ERROR_USER              17

// Returned to OS on exit after internal error.
#define ERROR_INTERNAL      12

//////////////////////
/// POINTER CHECKS ///
//////////////////////

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

// Argument in global 'x'.
extern lispptr           eval0           (void);
extern lispptr           eval            (void);
extern lispptr           eval_list       (void);
extern lispptr           funcall         (void);

extern void              gc               (void);
extern size_t            heap_free_size   (void);
extern lispobj_size_t FASTCALL objsize (char *);

#define REPL_STD        0
#define REPL_DEBUGGER   1
#define REPL_LOAD       2
extern lispptr  FASTCALL lisp_repl    (char mode, simpleio_chn_t);
extern bool     FASTCALL load         (char * pathname);

#ifndef NO_IMAGE
extern bool     FASTCALL image_load   (char * pathname);
extern bool     FASTCALL image_save   (char * pathname);
#endif

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
extern void     FASTCALL error               (char code, char * msg, lispptr info);
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

extern char con_reset (void);
extern char con_set   (char flags);

extern void init_builtins (void);
extern void init_eval     (void);
extern bool init_heap     (void);
extern void init_list     (void);
extern void init_onerror  (void);
extern void init_read     (void);
extern void init_repl     (void);
extern void heap_add_init_areas (void);

#endif // #ifndef __LIBLISP_H__
