// Atto Lisp interpreter

#ifndef __CBM__
#define __CBM__
#endif

#define HEAP_START  0x4000

#include <ingle/cc65-charmap.h>

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include <cbm.h>

#include <term/libterm.h>

typedef void * ptr;
typedef unsigned char uchar;

#define TYPE_CONS     1
#define TYPE_NUMBER   2
#define TYPE_SYMBOL   4
#define TYPE_BUILTIN  8

typedef struct _cons {
    uchar  size;
    uchar  type;
    ptr    car;
    ptr    cdr;
} cons;

typedef struct _number {
    uchar  size;
    uchar  type;
    int    value;
} number;

typedef struct _symbol {
    uchar  size;
    uchar  type;
    ptr    value;
    uchar  len;
} symbol;

#pragma bss-name (push, "ZEROPAGE")
char *    heap;
symbol *  s;
char      c;
char      do_putback;
ptr       nil;
#pragma zpsym ("heap");
#pragma zpsym ("s");
#pragma zpsym ("c")
#pragma zpsym ("do_putback")
#pragma zpsym ("nil")
#pragma bss-name (pop)

char token[256];

ptr __fastcall__
alloc (uchar size, uchar type)
{
    register char * r = heap;
    heap[0] = size;
    heap[1] = type;
    heap += size;
    heap[0] = 0;    // Mark end of heap.
    return r;
}

ptr __fastcall__
make_cons (ptr car, ptr cdr)
{
    register cons * c = alloc (sizeof (cons), TYPE_CONS);
    c->car = car;
    c->cdr = cdr;
    return c;
}

ptr __fastcall__
make_number (int x)
{
    number * n = alloc (sizeof (number), TYPE_NUMBER);
    n->value = x;
    return n;
}

void * __fastcall__
lookup_symbol (char * str, uchar len)
{
    char * h = (char *) HEAP_START;

    while (*h) {
        if (h[1] == TYPE_SYMBOL && h[4] == len && !memcmp (&h[5], str, len))
            return h;
        h += *h;
    }

    return NULL;
}

ptr __fastcall__
make_symbol (char * str, uchar len)
{
    symbol * s = lookup_symbol (str, len);
    if (s)
        return s;

    s = alloc (sizeof (symbol) + len, TYPE_SYMBOL);
    s->len = len;
    memcpy (&s->len + 1, str, len);
    return s;
}

char
eof ()
{
    return cbm_k_readst () & 0x40;
}

char
in ()
{
    if (do_putback) {
        do_putback = false;
        return c;
    }
    c = cbm_k_basin ();
    term_put (c);
    return c;
}

void
putback ()
{
    do_putback = true;
}

void
skip_spaces ()
{
    while (!eof ()) {
        if (!isspace (in ())) {
            putback ();
            return;
        }
    }
}

ptr read (void);

ptr
read_list ()
{
    cons * c;
    cons * start;
    cons * last = NULL;

    if (in () != '(') {
        term_puts ("ERROR: List expected.\n\r");
        while (1);
    }

    while (1) {
        skip_spaces ();
        if (eof ()) {
            term_puts ("ERROR: Missing closing bracket.\n\r");
            while (1);
        }
        if (in () == ')')
            return start;
        putback ();

        c = make_cons (read (), NULL);
        if (last)
            last->cdr = c;
        else
            start = c;
        last = c;
    }
}

ptr
read_number ()
{
    char * p = token;

    while (!eof () && isdigit (in ()))
        *p++ = c;
    *p = 0;
    putback ();

    return make_number (atoi (token));
}

bool __fastcall__
our_isalpha (char c)
{
    return c >= 'A' && c <= 'Z' ||
           c >= 'a' && c <= 'z';
}

ptr
read_symbol ()
{
    char * p = token;

    while (!eof () && our_isalpha (in ()))
        *p++ = c;
    putback ();

    return make_symbol (token, p - token);
}

ptr
read ()
{
    if (eof ())
        return NULL;

    skip_spaces ();
    c = in ();
    putback ();
    if (c == '(')
        return read_list ();
    if (isdigit (c))
        return read_number ();
    return read_symbol ();
}

struct builtin {
    char * name;
    void * func;
} builtins[] = {
    { "car", NULL },
    { "cdr", NULL }
};

int
main (int argc, char * argv[])
{
    (void) argc;
    (void) argv;

    term_init ();
    term_puts ("AttoLisp - loading environment...\n\r");

    heap = (void *) HEAP_START;
    heap[0] = 0;
    nil = make_symbol ("nil", 3);

    do_putback = false;
    cbm_open (3, 8, 3, "ENV.LISP");
    cbm_k_chkin (3);
    read ();
    cbm_k_close (3);

    term_puts ("Bye!\n\r");
    while (1);
    return 0;
}
