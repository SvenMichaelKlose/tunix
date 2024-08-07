#ifdef __CC65__
#ifndef __CBM__
#define __CBM__
#endif
#include <ingle/cc65-charmap.h>
#include <cbm.h>
#endif

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include <simpleio/libsimpleio.h>

#include "liblisp.h"

#define RELOC_TABLE_SIZE (RELOC_TABLE_ENTRIES * (sizeof (lispptr) + sizeof (unsigned)))
#ifdef GC_STRESS
#define NEEDS_GC()  do_gc_stress
#else
#define NEEDS_GC()  (heap_free >= heap_end - size)
#endif

// Heap memory areas  Must be consecutive!
#ifdef FRAGMENTED_HEAP
struct heap_fragment heaps[] = {
    { NULL, NULL, NULL },

    // Commodore VIC-20, BLK5
#ifdef TARGET_VIC20
    { (void *) 0xa000, (void *) 0xa000, (void *) 0xc000 },
#endif

    { NULL, NULL, NULL }  // End of heap list.
};
#endif // #ifdef FRAGMENTED_HEAP

lispptr t;
lispptr first_symbol;
lispptr last_symbol;
extern char * xlat_start;
extern char * xlat_end;

#ifdef FRAGMENTED_HEAP
struct heap_fragment * heap;
#endif

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
lispptr tmp2;
char tmpc;
char * heap_start;
char * heap_free;
char * heap_end;
char * h;
char * ptr;
char   type;
symbol *  sym;
#ifdef __CC65__
#pragma zpsym ("tmp2")
#pragma zpsym ("tmpc")
#pragma zpsym ("heap_start");
#pragma zpsym ("heap_free");
#pragma zpsym ("heap_end");
#pragma zpsym ("h");
#pragma zpsym ("ptr");
#pragma zpsym ("type");
#pragma zpsym ("sym");
#pragma bss-name (pop)
#endif

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern char do_putback;
#ifdef __CC65__
#pragma zpsym ("do_putback")
#pragma bss-name (pop)
#endif

lispptr universe;
char buffer[MAX_SYMBOL + 1];
uchar lisp_sizes[TYPE_EXTENDED * 2];

#ifdef GC_STRESS
bool do_gc_stress;
#endif

// Add object to root list of garbage collector.
void FASTCALL
expand_universe (lispptr x)
{
    SET_SYMBOL_VALUE(universe, make_cons (x, SYMBOL_VALUE(universe)));
}

// Get size of object in bytes.
unsigned FASTCALL
objsize (char * x)
{
    uchar s;
    s = lisp_sizes[TYPEBITS(x)];
#ifndef NDEBUG
    if (!s)
        internal_error ("0 size");
#endif
    if (_NAMEDP(x))
        return s + SYMBOL_LENGTH(x);
    return s;
}

#ifndef NDEBUG
#ifdef TARGET_UNIX

void
error_typebits (char * x)
{
    internal_error (x ? "type" : "EOH");
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
    printf ("Pointer %p is not on an object's start address.\n", x);
    internal_error (":(");
}

bool is_checking_lispptr;

// Check if object pointed to is sane.
void
check_lispptr (char * x)
{
    if (!x || is_checking_lispptr)
        return;

#ifndef FRAGMENTED_HEAP
    // Check if object is within heap.
    if (x < heap_start)
        internal_error ("Pointer below heap.");
    if (x >= heap_end)
        internal_error ("Pointer above heap.");

#ifdef TARGET_UNIX
    // Check if pointer is on an object start.
    check_lispptr_addr (x);
#endif
#endif // #ifndef FRAGMENTED_HEAP

    if (*x & (TYPE_UNUSED1 | TYPE_UNUSED2))
        internal_error ("Unused type bits set.");

    switch (TYPEBITS(x)) {
        case TYPE_CONS:
        case TYPE_NUMBER:
        case TYPE_SYMBOL:
        case TYPE_BUILTIN:
        case TYPE_SPECIAL:
            break;
        default:
            error_typebits (x);
    }
}

// Print object info.
void
dump_lispptr (char * x)
{
    int n = objsize (x);
    printf ("%p - %p %d: ", x, x + n - 1, n);
    is_checking_lispptr = true;
    if (!MARKED(x))
        printf ("(unused) ");
    if (!x) {
        printf ("nil\n");
        goto done;
    }
    switch (TYPEBITS(x)) {
        case TYPE_CONS:
            printf ("cons %p, %p\n", CAR(x), CDR(x));
            break;
        case TYPE_NUMBER:
            printf ("number %ld\n", NUMBER_VALUE(x));
            break;
        case TYPE_SYMBOL:
            printf ("symbol '");
            outsn (SYMBOL_NAME(x), SYMBOL_LENGTH(x));
            printf ("' %p\n", SYMBOL_VALUE(x));
            break;
        case TYPE_BUILTIN:
            printf ("built-in '");
            outsn (SYMBOL_NAME(x), SYMBOL_LENGTH(x));
            printf ("' %p\n", SYMBOL_VALUE(x));
            break;
        case TYPE_SPECIAL:
            printf ("special '");
            outsn (SYMBOL_NAME(x), SYMBOL_LENGTH(x));
            printf ("' %p\n", SYMBOL_VALUE(x));
            break;
        default:
            error_typebits (x);
    }
done:
    is_checking_lispptr = false;
}

// Print info of all objects.
void
dump_heap ()
{
    char *s = heap_start;
    while (*s) {
        dump_lispptr (s);
        s += objsize (s);
    }
}

#endif // #ifdef TARGET_UNIX
#endif // #ifndef NDEBUG

// Allocate object.
lispptr FASTCALL
alloc (uchar size, uchar type)
{
    if (NEEDS_GC()) {
        gc ();
#if !defined(NAIVE) && !defined(GC_STRESS)
        if (NEEDS_GC()) {
            error (ERROR_OUT_OF_HEAP, "Out of heap.");
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

// Make list cell.
lispptr FASTCALL
make_cons (lispptr car, lispptr cdr)
{
    PUSH(car);
    PUSH(cdr);
    tmp = alloc (sizeof (cons), TYPE_CONS);
    POP(cdr);
    POP(car);
    CONS(tmp)->car = car;
    CONS(tmp)->cdr = cdr;
    return tmp;
}

// Make number object.
lispptr FASTCALL
make_number (lispnum_t x)
{
    tmp = alloc (sizeof (number), TYPE_NUMBER);
    NUMBER(tmp)->value = x;
    return tmp;
}

// Look up symbol by name.
void * FASTCALL
lookup_symbol (char * str, uchar len)
{
    // Walk over singly-linked list of named symbols.
    // (Anonymous symbols have no name.)
    for (ptr = first_symbol; ptr; ptr = SYMBOL_NEXT(ptr))
        if (SYMBOL_LENGTH(ptr) == len
            && !memcmp (ptr + sizeof (symbol), str, len))
            return ptr;
    return NULL;
}

// Make symbol object.
lispptr FASTCALL
alloc_symbol (char * str, uchar len)
{
    tmp = alloc (sizeof (symbol) + len, TYPE_SYMBOL);
    if (len) {
        if (last_symbol)
            SYMBOL_NEXT(last_symbol) = tmp;
        last_symbol = tmp;
    }
    SYMBOL(tmp)->next = nil;
    SYMBOL(tmp)->value = tmp;
    SYMBOL(tmp)->length = len;
    memcpy ((char *) tmp + sizeof (symbol), str, len);
    return tmp;
}

// Find or create symbol.
lispptr FASTCALL
make_symbol (char * str, uchar len)
{
    if (!(tmp = lookup_symbol (str, len)))
        return alloc_symbol (str, len);
    return tmp;
}

bool
init_heap ()
{
    size_t heap_size;
    uchar i;

    // Make object size table.
    for (i = 0; i < TYPE_EXTENDED * 2; i++)
        lisp_sizes[i] = 0;
    lisp_sizes[TYPE_CONS] = sizeof (cons);
    lisp_sizes[TYPE_NUMBER] = sizeof (number);
    lisp_sizes[TYPE_SYMBOL] = sizeof (symbol);
    lisp_sizes[TYPE_BUILTIN] = sizeof (symbol);
    lisp_sizes[TYPE_SPECIAL] = sizeof (symbol);

    // Allocate tag stack.
#ifdef MALLOCD_TAGSTACK
    tagstack_start = malloc (TAGSTACK_SIZE);
    tagstack = tagstack_start + TAGSTACK_SIZE;
#else
    tagstack_start = (void *) TAGSTACK_START;
    tagstack = (void *) TAGSTACK_END;
#endif
    tagstack_end = tagstack;

    // Allocate object stack.
#ifdef MALLOCD_STACK
    stack_start = malloc (STACK_SIZE);
    if (!stack_start)
        return false;
    stack_end = stack_start + STACK_SIZE;
#else
    stack_start = (void *) STACK_START;
    stack_end = (void *) STACK_END;
#endif
    stack = stack_end;

    // Allocate relocation table.
    xlat_start = malloc (RELOC_TABLE_SIZE);
    if (!xlat_start)
        return false;
    xlat_end = xlat_start + RELOC_TABLE_SIZE;

    // Allocate heap.
#ifdef __CC65__
    heap_size = _heapmaxavail ();
#else
    heap_size = HEAP_SIZE;
#endif
    if (!(heap_start = malloc (heap_size)))
        return false;
    heap_free = heap_start;
    heap_end = heap_start + heap_size;

#ifdef FRAGMENTED_HEAP
    // Update descriptor of malloc()'ed heap.
    heaps[0].start = heaps[0].free = heap_start;
    heaps[0].end = heap_end;

    // Mark ends of heaps.
    for (heap = heaps; heap->start; heap++)
        *(heap->free) = 0;

    // Cause gc() before first allocation to switch to
    // the first heap.
    heap_free = heap_end;
    heap = heaps;
#else
    // Mark end of heap.
    *heap_free = 0;
#endif

#ifdef GC_STRESS
    // We need to wait until inits are done.
    do_gc_stress = false;
#endif

    // Make universe.
    last_symbol = nil;
    universe = make_symbol ("*universe*", 10);
    first_symbol = universe;
    t = make_symbol ("t", 1);
    SET_SYMBOL_VALUE(universe, make_cons (t, nil));

    // Make +target+.
    tmp2 = make_symbol ("+target+", 8);
    expand_universe (tmp2);
#ifdef TARGET_C128
    SET_SYMBOL_VALUE(tmp2, make_symbol ("c128", 4));
#endif
#ifdef TARGET_C16
    SET_SYMBOL_VALUE(tmp2, make_symbol ("c16", 3));
#endif
#ifdef TARGET_C64
    SET_SYMBOL_VALUE(tmp2, make_symbol ("c64", 3));
#endif
#ifdef TARGET_PET
    SET_SYMBOL_VALUE(tmp2, make_symbol ("pet", 3));
#endif
#ifdef TARGET_PLUS4
    SET_SYMBOL_VALUE(tmp2, make_symbol ("plus4", 5));
#endif
#ifdef TARGET_VIC20
    SET_SYMBOL_VALUE(tmp2, make_symbol ("vic20", 5));
#endif
#ifdef TARGET_UNIX
    SET_SYMBOL_VALUE(tmp2, make_symbol ("unix", 4));
#endif

    // Init input.
    // TODO: Move (pixel)
    do_putback = false;

    // Clear error info.
#ifndef NAIVE
#ifndef NDEBUG
    debug_mode = false;
#endif
    error_code = false;
    last_errstr = NULL;
    current_expr  = nil;
#endif

    return true;
}
