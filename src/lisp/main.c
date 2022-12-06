// Atto Lisp interpreter

#define HEAP_START  0x4000

#include <ingle/cc65-charmap.h>

#include <ctype.h>
#include <string.h>
#include <stdio.h>

#include <term/libterm.h>

typedef void * ptr;
typedef unsigned char uchar;

char * heap;

#define TYPE_CONS     1
#define TYPE_NUMBER   2
#define TYPE_SYMBOL   4
#define TYPE_BUILTIN  8

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

typedef struct _cons {
    uchar  size;
    uchar  type;
    ptr    car;
    ptr    cdr;
} cons;

ptr __fastcall__
make_cons (ptr car, ptr cdr)
{
    register cons * c = alloc (sizeof (cons), TYPE_CONS);
    c->car = car;
    c->cdr = cdr;
    return c;
}

typedef struct _number {
    uchar  size;
    uchar  type;
    int    value;
} number;

ptr __fastcall__
make_number (int x)
{
    number * n = alloc (sizeof (number), TYPE_NUMBER);
    n->value = x;
    return n;
}

typedef struct _symbol {
    uchar  size;
    uchar  type;
    ptr    value;
    uchar  len;
} symbol;

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

#pragma bss-name (push, "ZEROPAGE")
symbol * s;
#pragma zpsym ("s");
#pragma bss-name (pop);

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

char c;

char
eof ()
{
    return 0;
}

char
in ()
{
    return 0;
}

void
putback ()
{
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

ptr
read_list ()
{
    return NULL;
}

ptr
read_number ()
{
    return NULL;
}

ptr
read_symbol ()
{
    return NULL;
}

ptr
read ()
{
    if (eof ())
        return NULL;

    skip_spaces ();
    c = in ();
    if (c == '(')
        return read_list ();
    putback ();
    if (isdigit (c))
        return read_number ();
    return read_symbol ();
}

struct builtin {
    char * name;
    void * func;
} builtins[] = {
    { "CAR", NULL },
    { "CDR", NULL }
};

int
main (int argc, char * argv[])
{
    symbol * s1;
    symbol * s2;

    (void) argc;
    (void) argv;

    term_init ();
    term_puts ("AttoLisp\n\r");

    heap = (void *) HEAP_START;
    heap[0] = 0;
    make_cons ((ptr) 0xaa, (ptr) 0xdd);
    make_number (0x1234);
    make_symbol ("foo", 3);

    s1 = make_symbol ("lisp", 4);
    s2 = make_symbol ("lisp", 4);
    if (s1 != s2) {
        term_puts ("Error: double symbol.");
    }

    return 0;
}
