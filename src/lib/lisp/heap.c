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

#include <simpleio/libsimpleio.h>

#include "liblisp.h"

lispptr first_symbol;
lispptr last_symbol;

lispptr t;

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
lispptr tmp;
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
#pragma zpsym ("tmp")
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

unsigned lisp_sizes[] = {
    0,
    sizeof (cons),
    sizeof (number),
    sizeof (symbol),
    sizeof (symbol)
};

void FASTCALL
expand_universe (lispptr x)
{
    PUSH(x);
    universe = make_cons (x, universe);
    POP(x);
}

unsigned FASTCALL
objsize (char * x)
{
    uchar type = *x & 7; // TODO: named constant
    unsigned s;
#ifndef NDEBUG
    if (type > TYPE_MAX) {
        setout (STDERR);
        outs ("No size for type ");
        out_number (type);
        while (1);
    }
#endif // #ifndef NDEBUG
    s = lisp_sizes[type];
    if (*x & TYPE_NAMED)
        return s + SYMBOL_LENGTH(x);
    return s;
}

#define MIN_RELOC_TABLE_SIZE \
    ((sizeof (lispptr) + sizeof (unsigned)) * MIN_RELOC_TABLE_ENTRIES)
#define NEEDS_GC() \
    (heap_free > heap_end - size - MIN_RELOC_TABLE_SIZE)

// Allocate object.
lispptr FASTCALL
alloc (uchar size, uchar type)
{
    if (NEEDS_GC()) {
        gc ();
        if (NEEDS_GC()) {
            error ("Out of heap.");
            return nil;
        }
    }

    h = heap_free;
    *heap_free = type;
    heap_free += size;
    *heap_free = 0;

    return h;
}

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

lispptr FASTCALL
make_number (lispnum_t x)
{
    tmp = alloc (sizeof (number), TYPE_NUMBER);
    NUMBER(tmp)->value = x;
    return tmp;
}

void * FASTCALL
lookup_symbol (char * str, uchar len)
{
    for (ptr = first_symbol; ptr; ptr = SYMBOL_NEXT(ptr))
        if (SYMBOL_LENGTH(ptr) == len
            && !memcmp (ptr + sizeof (symbol), str, len))
            return ptr;
    return NULL;
}

lispptr FASTCALL
alloc_symbol (char * str, uchar len)
{
    tmp = alloc (sizeof (symbol) + len, TYPE_SYMBOL);
    if (len) {
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
    tmp = lookup_symbol (str, len);
    if (!tmp)
        return alloc_symbol (str, len);
    return tmp;
}

bool
lisp_init ()
{
    size_t heap_size;

    // Make tag stack.
#ifdef TARGET_VIC20
    tagstack_start = (void *) 0x0400;
    tagstack = (void *) 0x0800;
#else
    tagstack_start = malloc (TAGSTACK_SIZE);
    tagstack = tagstack_start + TAGSTACK_SIZE;
#endif
    tagstack_end = tagstack;

    // Make object stack.
#ifdef TARGET_VIC20
    stack_start = (void *) 0x0800;
    stack_end = (void *) 0x1000;
#else
    stack_start = malloc (STACK_SIZE);
    if (!stack_start)
        return false;
    stack_end = stack_start + STACK_SIZE;
#endif
    stack = stack_end;

    // Make heap.
#ifdef __CC65__
    heap_size = _heapmaxavail ();
#else
    heap_size = HEAP_SIZE;
#endif
    heap_start = heap_free = malloc (heap_size);
    if (!heap_start)
        return false;
    *heap_free = 0;
    heap_end = heap_start + heap_size;

    // Make universe with essential symbols.
    last_symbol = heap_free;
    t = first_symbol = last_symbol = make_symbol ("t", 1);
    universe = make_cons (t, nil);
    delayed_eval = make_symbol ("%E", 2);
    expand_universe (delayed_eval);
    block_sym   = make_symbol ("block", 5);
    expand_universe (block_sym);
    return_sym  = make_symbol (NULL, 0);
    expand_universe (return_sym);
    go_sym      = make_symbol (NULL, 0);
    expand_universe (go_sym);

    // Init input. TODO: Remove (pixel)
    do_putback = false;

    // Clear error info.
    debug_mode  = false;
    has_error   = false;
    last_errstr = NULL;
    last_eval_expr  = nil;

    return true;
}
