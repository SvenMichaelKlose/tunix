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

        c = make_cons (read (), nil);
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
    return !isspace (c) && (c < '0' || c > '9') && c != '(' && c != ')';
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
    skip_spaces ();
    if (eof ())
        return NULL;
    c = in ();
    putback ();
    if (c == '(')
        return read_list ();
    if (isdigit (c))
        return read_number ();
    return read_symbol ();
}

void
out (char c)
{
    term_put (c);
}

void
out_number (int n)
{
    int a;

    if (n > 9) {
        a = n / 10;
        n -= 10 * a;
        out_number (a);
    }
    out ('0' + n);
}

void print (ptr x);

void
print_list (cons * c)
{
    bool first = true;

    out ('(');
    while (c != nil) {
        if (!first)
            out (' ');
        else
            first = false;
        print (c->car);
        c = c->cdr;
    }
    out (')');
}

void
print_number (number * n)
{
    out_number (n->value);
}

void
print_symbol (symbol * s)
{
    char * p = (char *) &s->len + 1;
    uchar len = s->len;
    uchar i;

    for (i = 0; i < len; i++)
        out (*p++);
}

void
print (ptr x)
{
    switch (((char *) x)[1]) {
        case TYPE_CONS:
            print_list ((cons *) x);
            return;
        case TYPE_NUMBER:
            print_number ((number *) x);
            return;
        case TYPE_SYMBOL:
            print_symbol ((symbol *) x);
            return;
    }
    term_puts ("Unknown object type.");
    while (1);
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
    ptr env;

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
    while (env = read ()) {
        print (env);
        term_puts ("\n\r");
    }
    cbm_k_close (3);

    term_puts ("\n\rBye!\n\r");
    while (1);
    return 0;
}
