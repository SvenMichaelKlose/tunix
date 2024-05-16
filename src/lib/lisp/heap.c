// Object heap
//
// There is no garbage collection.  The heap is growing
// upwards.

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

#include "liblisp.h"

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
char *    heap;
symbol *  s;
lispptr       nil;
lispptr       t;
#ifdef __CC65__
#pragma zpsym ("heap");
#pragma zpsym ("s");
#pragma zpsym ("nil")
#pragma zpsym ("t")
#pragma bss-name (pop)
#endif

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern char      do_putback;
#ifdef __CC65__
#pragma zpsym ("do_putback")
#pragma bss-name (pop)
#endif

// Allocate vanilla object.
lispptr __fastcall__
alloc (uchar size, uchar type)
{
    char * r = heap;
    *r = type;
    heap += size;
    *heap = 0;  // New end of heap.
    return r;
}

lispptr __fastcall__
lisp_make_cons (lispptr car, lispptr cdr)
{
    cons * c = alloc (sizeof (cons), TYPE_CONS);
    c->car = car;
    c->cdr = cdr;
    return c;
}

lispptr __fastcall__
lisp_make_number (int x)
{
    number * n = alloc (sizeof (number), TYPE_NUMBER);
    n->value = x;
    return n;
}

char sizes[] = {
    0,
    sizeof (cons),
    sizeof (number),
    sizeof (symbol)
};

void * __fastcall__
lookup_symbol (char * str, uchar len)
{
    char *    s = (char *) HEAP_START;
    char      type;
    symbol *  sym;

    // Check all objects until end of heap.
    while ((type = *s)) {
        if (type & TYPE_NAMED) {
            sym = (symbol *) s;

            // Return match.
            if (sym->len == len
                && !memcmp (s + sizeof (symbol), str, len))
                return s;

            // Jump over symbol + name.
            s += sizeof (symbol) + sym->len;
            continue;
        }

        // Jump over current object.
        s += sizes[*s];
    }

    return NULL;
}

lispptr __fastcall__
lisp_make_symbol (char * str, uchar len)
{
    symbol * s;

    // Return existing.
    if ((s = lookup_symbol (str, len)))
        return s;

    // Alloc new.
    s = alloc (sizeof (symbol) + len, TYPE_SYMBOL);
    s->value = s;
    s->len = len;
    memcpy ((char *) s + sizeof (symbol), str, len);
    return s;
}

void
lisp_init ()
{
    // Empty heap.
    heap = (char *) HEAP_START;
    *heap = 0;

    // Make truth.
    nil = lisp_make_symbol ("nil", 3);
    t   = lisp_make_symbol ("t", 1);

    // Init input.
    do_putback = false;
}
