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

//#include <term/libterm.h>

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

lispptr __fastcall__
alloc (uchar size, uchar type)
{
    char * r = heap;
    heap[0] = size;
    heap[1] = type;
    heap += size;
    heap[0] = 0;    // Mark end of heap (size field).
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

void * __fastcall__
lookup_symbol (char * str, uchar len)
{
    symbol * s = (void *) HEAP_START;

    while (s->size) {
        if (s->type == TYPE_SYMBOL
            && s->len == len
            && !memcmp (&s->name, str, len))
            return s;
        s = (symbol *) &s->name + len;
    }

    return NULL;
}

lispptr __fastcall__
lisp_make_symbol (char * str, uchar len)
{
    symbol * s;
    if ((s = lookup_symbol (str, len)))
        return s;
    s = alloc (sizeof (symbol) + len, TYPE_SYMBOL);
    s->value = s;
    s->bind = nil;
    s->len = len;
    memcpy (&s->len + 1, str, len);
    return s;
}

void
lisp_init ()
{
    heap = (void *) HEAP_START;
    heap[0] = 0;
    nil = lisp_make_symbol ("nil", 3);
    t   = lisp_make_symbol ("t", 2);
    do_putback = false;
}
