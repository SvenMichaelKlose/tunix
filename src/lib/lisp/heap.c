#ifdef __CC65__
#ifndef __CBM__
#define __CBM__
#endif
#include <ingle/cc65-charmap.h>
#include <cbm.h>
#endif

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <setjmp.h>

#include <simpleio/libsimpleio.h>

#include "liblisp.h"

#ifdef GC_STRESS
    #define NEEDS_GC(size)  do_gc_stress
#else
    // Object size + end-of-heap marker (0).
    #define NEEDS_GC(size)  (heap_free + size + onetime_heap_margin + 1 >= heap_end)
#endif

// Heap memory areas  Must be consecutive!
#ifdef FRAGMENTED_HEAP
struct heap_fragment heaps[] = {
#ifdef TARGET_VIC20
    { (void *) 0x0900, (void *) 0x0900, (void *) 0x0c00 },
#endif

    // Regular heap.
    { NULL, NULL, NULL },

    // Commodore VIC-20, BLK5
#ifdef TARGET_VIC20
    { (void *) 0xa000, (void *) 0xa000, (void *) 0xc000 },
#endif

    { NULL, NULL, NULL },

    // End of heap marker.
    { NULL, NULL, NULL }
};
#endif // #ifdef FRAGMENTED_HEAP

lispptr  t;
lispptr  first_symbol;
lispptr  last_symbol;

#if !defined(NO_VERBOSE_LOAD) && !defined(NO_VERBOSE_DEFINES)
lispptr  vp_symbol;
#endif

size_t  onetime_heap_margin;

#ifdef FRAGMENTED_HEAP
struct heap_fragment * heap;
#endif

char *   heap_start;

#ifdef USE_ZEROPAGE
#pragma bss-name (push, "ZEROPAGE")
#endif
lispptr  tmp;
lispptr  tmp2;
char     tmpc;
char *   tmpstr;
char *   heap_free;
char *   heap_end;
char *   h;
char     type;
symbol * sym;
#ifdef NIL_NOT_0
struct real_nil real_nil;
#endif
#ifdef USE_ZEROPAGE
#pragma zpsym ("h");
#pragma zpsym ("type");
#pragma zpsym ("sym");
#pragma bss-name (pop)
#endif // #ifdef USE_ZEROPAGE

lispptr universe;
char    buffer[MAX_SYMBOL + 1];
uchar   lisp_sizes[TYPE_EXTENDED * 2];

#ifdef GC_STRESS
bool    do_gc_stress;
#endif

#ifdef __CC65__
#pragma code-name ("CODE_HEAP")
#endif

// Add object to root list of garbage collector.
void FASTCALL
expand_universe (lispptr x)
{
    SET_SYMBOL_VALUE(universe, make_cons (x, SYMBOL_VALUE(universe)));
}

// Get size of object in bytes.
lispobj_size_t FASTCALL
objsize (char * x)
{
    uchar s = lisp_sizes[TYPEBITS(x)];
#ifndef NDEBUG
    if (!s)
        internal_error_ptr (x, "0 size");
#endif
    if (_NAMEDP(x))
        return s + SYMBOL_LENGTH(x);
    return s;
}

#ifdef CHECK_OBJ_POINTERS

void
error_typebits (char * x)
{
    internal_error_ptr (x, x ? "type" : "EOH");
}

// Print info of all objects.
void
check_lispptr_addr (char * x)
{
    char * s = heap_start;
    while (s != heap_end) {
        if (s == x)
            return;
        s += objsize (s);
    }
    internal_error_ptr (x, "Not obj addr");
}

bool is_checking_lispptr;

// Check if object pointed to is sane.
void
check_lispptr (char * x)
{
    if (NOT(x) || is_checking_lispptr)
        return;

#ifndef FRAGMENTED_HEAP
    // Check if object is within heap.
    if (x < heap_start)
        internal_error_ptr (x, "Pointer below heap.");
    if (x >= heap_free)
        internal_error_ptr (x, "Pointer to free heap.");
    if (x >= heap_end)
        internal_error_ptr (x, "Pointer above heap.");

#ifdef TARGET_UNIX
    // Check if pointer is on an object start.
    check_lispptr_addr (x);
#endif
#endif // #ifndef FRAGMENTED_HEAP

    if (*x & (TYPE_UNUSED1 | TYPE_UNUSED2))
        internal_error_ptr (x, "Unused type bits set.");

    switch (TYPEBITS(x)) {
        case TYPE_CONS:
#ifdef COMPRESSED_CONS
        case TYPE_CONS | TYPE_EXTENDED:
#endif
        case TYPE_NUMBER:
        case TYPE_SYMBOL:
        case TYPE_BUILTIN:
        case TYPE_SPECIAL:
            break;
        default:
            error_typebits (x);
    }
}

#endif // #ifdef CHECK_OBJ_POINTERS

#ifdef DUMP_LISPPTR

// Print object info.
void
dump_lispptr (char * x)
{
    int n;
    if (!x) {
        printf ("0 - 0 0: nil\n");
        return;
    }
    n = objsize (x);
    printf ("%p - %p %d: ", x, x + n - 1, n);
#ifdef CHECK_OBJ_POINTERS
    is_checking_lispptr = true;
#endif
    if (!MARKED(x))
        printf ("(unused) ");
    if (!x) {
        printf ("nil\n");
        goto done;
    }
    switch (TYPEBITS(x)) {
        case TYPE_CONS:
#ifdef COMPRESSED_CONS
            if (_EXTENDEDP(x))
                printf ("ccons %p\n", CAR(x));
            else
#endif
            printf ("cons %p, %p\n", CAR(x), CDR(x));
            break;
        case TYPE_NUMBER:
            printf ("number %ld\n", NUMBER_VALUE(x));
            break;
        case TYPE_SYMBOL:
            printf ("symbol '");
            outm (SYMBOL_NAME(x), SYMBOL_LENGTH(x));
            printf ("' %p\n", SYMBOL_VALUE(x));
            break;
        case TYPE_BUILTIN:
            printf ("built-in '");
            outm (SYMBOL_NAME(x), SYMBOL_LENGTH(x));
            printf ("' %p\n", SYMBOL_VALUE(x));
            break;
        case TYPE_SPECIAL:
            printf ("special '");
            outm (SYMBOL_NAME(x), SYMBOL_LENGTH(x));
            printf ("' %p\n", SYMBOL_VALUE(x));
            break;
        default:
            error_typebits (x);
    }
done:
#ifdef CHECK_OBJ_POINTERS
    is_checking_lispptr = false;
#endif
}

// Print info of all objects.
void
dump_heap ()
{
    char * s = heap_start;
    while (*s) {
        dump_lispptr (s);
        s += objsize (s);
    }
}

#endif // #ifdef DUMP_LISPPTR

#ifdef __CC65__
#pragma codesize (push, 500)
#endif

// Allocate object.
lispptr FASTCALL
alloc (uchar size, uchar type)
{
    if (NEEDS_GC(size)) {
        gc ();
#if !defined(NAIVE) && !defined(GC_STRESS)
        if (NEEDS_GC(size)) {
            onetime_heap_margin = 0;
            error (ERROR_OUT_OF_HEAP, "Out of heap.");
            longjmp (*hard_repl_break, 1);
            return nil;
        }
#endif
    }

    h = heap_free;
    *heap_free = type;
    heap_free += size;
    *heap_free = 0;

    return h;
}

lispptr make_cons_tmp;
lispptr make_cons_car;
lispptr make_cons_cdr;

// Make list cell.
lispptr FASTCALL
make_cons (lispptr car, lispptr cdr)
{
    make_cons_car = car;
    make_cons_cdr = cdr;
    make_cons_tmp = alloc (sizeof (cons), TYPE_CONS);
    CONS(make_cons_tmp)->car = make_cons_car;
    CONS(make_cons_tmp)->cdr = make_cons_cdr;
    return make_cons_tmp;
}

// Make number object.
lispptr FASTCALL
make_number (lispnum_t x)
{
    tmp = alloc (sizeof (number), TYPE_NUMBER);
    NUMBER(tmp)->value = x;
    return tmp;
}

#define IS_MATCHING_SYMNAME(sym, name, len) \
    (SYMBOL_LENGTH(sym) == len && !memcmp (SYMBOL_NAME(sym), name, len))

// Look up symbol by name.
void * FASTCALL
lookup_symbol (char * name, uchar len)
{
    // Walk over singly-linked list of named symbols.
    // (Anonymous symbols have no name.)
    for (tmpstr = first_symbol; NOT_NIL(tmpstr); tmpstr = SYMBOL_NEXT(tmpstr))
        if (IS_MATCHING_SYMNAME(tmpstr, name, len))
            return tmpstr;
    return NULL;
}

// Make symbol object.
lispptr FASTCALL
alloc_symbol (char * str, uchar len)
{
    tmp = alloc (sizeof (symbol) + len, TYPE_SYMBOL);
    if (len) {
        if (NOT_NIL(last_symbol))
            SYMBOL_NEXT(last_symbol) = tmp;
        last_symbol = tmp;
    }
    SYMBOL(tmp)->value  = tmp;
    SYMBOL(tmp)->next   = nil;
    SYMBOL(tmp)->length = len;
    memcpy ((char *) tmp + sizeof (symbol), str, len);
    return tmp;
}

// Find or create symbol.
lispptr FASTCALL
make_symbol (char * str, uchar len)
{
    tmp = lookup_symbol (str, len);
    if (NOT(tmp))
        return alloc_symbol (str, len);
    return tmp;
}

#ifdef FRAGMENTED_HEAP
// Switch to 'heap'.
void
switch_heap ()
{
    heap_start = heap->start;
    heap_free  = heap->free;
    heap_end   = heap->end;
    heap++;
}
#endif

size_t
heap_free_size ()
{
#ifdef FRAGMENTED_HEAP
    struct heap_fragment *h;
    size_t freed = 0;
    for (h = heaps; h->start; h++)
        freed += h->end - h->free;
    return freed;
#else
    return heap_end - heap_free;
#endif
}

#ifdef WAS_TARGET_VIC20

extern char * _CODE_INIT_RUN__;
extern size_t _CODE_INIT_SIZE__;
extern size_t _RODATA_INIT_SIZE__;

#pragma code-name ("CODE")

void
heap_add_init_areas (void)
{
    *_CODE_INIT_RUN__ = 0; // End-of-heap marker.
    switch_heap ();
    memcpy (&heaps[2], &heaps[1], sizeof (struct heap_fragment));
    memcpy (&heaps[1], &heaps[0], sizeof (struct heap_fragment));
    heap[0].start = heap[0].free = _CODE_INIT_RUN__;
    heap[0].end   = _CODE_INIT_RUN__ + _CODE_INIT_SIZE__;
    heap_free = heap_end;
    heap = &heaps[3];
}

#endif // #ifdef WAS_TARGET_VIC20

#ifdef __CC65__
#pragma codesize (pop)
#pragma code-name ("CODE_INIT")
#pragma inline-stdfuncs (off)
#pragma allow-eager-inline (off)
#endif

bool
init_heap ()
{
    size_t heap_size;
    uchar i;
#ifdef REAL_NIL
    symbol * s;
#endif

    // Make object size table.
    for (i = 0; i < TYPE_EXTENDED * 2; i++)
        lisp_sizes[i] = 0;
    lisp_sizes[TYPE_CONS] = sizeof (cons);
#ifdef COMPRESSED_CONS
    lisp_sizes[TYPE_CONS | TYPE_EXTENDED] = sizeof (ccons);
#endif
    lisp_sizes[TYPE_NUMBER]  = sizeof (number);
    lisp_sizes[TYPE_SYMBOL]  = sizeof (symbol);
    lisp_sizes[TYPE_BUILTIN] = sizeof (symbol);
    lisp_sizes[TYPE_SPECIAL] = sizeof (symbol);

#ifdef REAL_NIL
    // Make real NIL.
    s = (symbol *) nil;
    s->type   = TYPE_SYMBOL;
    s->length = 3;
    s->value  = nil;
    s->next   = nil;
    tmpstr = SYMBOL_NAME((lispptr) s);
    tmpstr[0] = 'n';
    tmpstr[1] = 'i';
    tmpstr[2] = 'l';
#endif // #ifdef REAL_NIL

    // Allocate tag stack.
#ifdef MALLOCD_TAGSTACK
    tagstack_start = malloc (TAGSTACK_SIZE);
    tagstack       = tagstack_start + TAGSTACK_SIZE;
#else
    tagstack_start = (void *) TAGSTACK_START;
    tagstack       = (void *) TAGSTACK_END;
#endif
    tagstack_end = tagstack;
    // Avoid trashing rest of program on overflow.
    tagstack_start += 8 * sizeof (lispptr);

    // Allocate object stack.
#ifdef MALLOCD_STACK
    stack_start = malloc (STACK_SIZE);
    if (!stack_start)
        return false;
    stack_end = stack_start + STACK_SIZE - 100;
#else
    stack_start = (void *) STACK_START;
    stack_end   = (void *) STACK_END;
#endif
    stack = stack_end;
    // Avoid trashing rest of program on overflow.
    stack_start += 8 * sizeof (lispptr);

    // Allocate relocation table.
    xlat_start = malloc (RELOC_TABLE_ENTRIES * sizeof (xlat_item));
    if (!xlat_start)
        return false;
    xlat_end = xlat_start + RELOC_TABLE_ENTRIES;

    // Allocate heap.
#ifdef __CC65__
    heap_size = _heapmaxavail ();
#else
    heap_size = HEAP_SIZE;
#endif
    if (!(heap_start = malloc (heap_size)))
        return false;
    heap_free = heap_start;
    heap_end  = heap_start + heap_size;

#ifdef FRAGMENTED_HEAP
#ifndef TARGET_VIC20
#error "FRAGMENTED_HEAP requires TARGET_VIC20"
#endif
    // Update descriptor of malloc()'ed heap.
    heaps[1].start = heaps[1].free = heap_start;
    heaps[1].end   = heap_end;

    // Mark ends of heaps.
    for (heap = heaps; heap->start; heap++)
        *(heap->free) = 0;

    // Trigger gc() with first allocation to start with the
    // first heap.
    heap_free = heap_end;
    heap = heaps;
#else
    // Mark end of heap.
    *heap_free = 0;
#endif

    onetime_heap_margin = ONETIME_HEAP_MARGIN;

#ifdef GC_STRESS
    // We need to wait until inits are done.
    do_gc_stress = false;
#endif

#ifdef COMPRESSED_CONS
    do_compress_cons = false;
#endif

    // Make first symbol *UNIVERSE*.
    first_symbol = last_symbol = nil;
    universe     = alloc_symbol ("*universe*", 10);
    first_symbol = universe;

    // Be true.
    t = alloc_symbol ("t", 1);

    // Make +TARGET+.
    tmp2 = alloc_symbol ("+target+", 8);
    SET_SYMBOL_VALUE(universe, make_cons (tmp2, nil));
#ifdef TARGET_C128
    SET_SYMBOL_VALUE(tmp2, alloc_symbol ("c128", 4));
#endif
#ifdef TARGET_C16
    SET_SYMBOL_VALUE(tmp2, alloc_symbol ("c16", 3));
#endif
#ifdef TARGET_C64
    SET_SYMBOL_VALUE(tmp2, alloc_symbol ("c64", 3));
#endif
#ifdef TARGET_PET
    SET_SYMBOL_VALUE(tmp2, alloc_symbol ("pet", 3));
#endif
#ifdef TARGET_PLUS4
    SET_SYMBOL_VALUE(tmp2, alloc_symbol ("plus4", 5));
#endif
#ifdef TARGET_VIC20
    SET_SYMBOL_VALUE(tmp2, alloc_symbol ("vic20", 5));
#endif
#ifdef TARGET_UNIX
    SET_SYMBOL_VALUE(tmp2, alloc_symbol ("unix", 4));
#endif

    // Define verbosity flag *V?*.
#if !defined(NO_VERBOSE_LOAD) && !defined(NO_VERBOSE_DEFINES)
    vp_symbol = alloc_symbol ("*v?*", 4);
    SET_SYMBOL_VALUE(vp_symbol, t);
    expand_universe (vp_symbol);
#endif

    // Clear error info.
#ifndef NAIVE
#ifndef NDEBUG
    debug_mode   = false;
#endif
    error_code   = false;
    last_errstr  = NULL;
    current_expr = nil;
#endif

#ifdef CHECK_OBJ_POINTERS
    is_checking_lispptr = false;
#endif

    return true;
}
