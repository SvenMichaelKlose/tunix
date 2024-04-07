#ifndef __CBM__
#define __CBM__
#endif

#define HEAP_START  0x5000

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

#define PTRTYPE(x)  (((char *) x)[1])
#define CONSP(x)    (PTRTYPE(x) & TYPE_CONS)

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
    ptr    bind;
    uchar  len;
} symbol;

//#pragma bss-name (push, "ZEROPAGE")
char *    heap;
symbol *  s;
char      c;
char      do_putback;
ptr       nil;
ptr       t;
ptr       args;
ptr       arg1;
ptr       arg2;
//#pragma zpsym ("heap");
//#pragma zpsym ("s");
//#pragma zpsym ("c")
//#pragma zpsym ("do_putback")
//#pragma zpsym ("nil")
//#pragma zpsym ("t")
//#pragma zpsym ("args")
//#pragma zpsym ("arg1")
//#pragma zpsym ("arg2")
//#pragma bss-name (pop)

#define NOTP(x)     (x == nil)

char token[256];

ptr __fastcall__
alloc (uchar size, uchar type)
{
    char * r = heap;
    heap[0] = size;
    heap[1] = type;
    heap += size;
    heap[0] = 0;    // Mark end of heap.
    return r;
}

ptr __fastcall__
make_cons (ptr car, ptr cdr)
{
    cons * c = alloc (sizeof (cons), TYPE_CONS);
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
        if (h[1] == TYPE_SYMBOL && h[6] == len && !memcmp (&h[7], str, len))
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
    s->value = s;
    s->bind = nil;
    s->len = len;
    memcpy (&s->len + 1, str, len);
    return s;
}

void
error (char * str)
{
    term_puts ("ERROR: ");
    term_puts (str);
    while (1);
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
        if (in () == ';') {
            while (!eof () && in () >= ' ');
            while (!eof () && in () < ' ');
            putback ();
            continue;
        }
        if (!isspace (c)) {
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

    if (in () != '(')
        error ("List expected.");

    while (1) {
        skip_spaces ();
        if (eof ())
            error ("Missing closing bracket.");
        if (in () == ')')
            return start;
        putback ();

        skip_spaces ();
        if (eof ())
            error ("Missing closing bracket.");
        if (in () == '.')
            c = read ();
        else {
            putback ();
            c = make_cons (read (), nil);
        }
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
    return !isspace (c) && c != '(' && c != ')';
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

void
outs (char * s)
{
    term_puts (s);
}

void
outsn (char * s, char len)
{
    term_putsn (s, len);
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
        if (c->cdr != nil && !CONSP(c->cdr)) {
            outs (" . ");
            print (c->cdr);
            break;
        }
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
    term_putsn ((char *) &s->len + 1, s->len);
}

void
print (ptr x)
{
    uchar type = PTRTYPE(x);

    if (type & TYPE_CONS)
        print_list ((cons *) x);
    else if (type & TYPE_NUMBER)
        print_number ((number *) x);
    else if (type & TYPE_SYMBOL)
        print_symbol ((symbol *) x);
    else
        error ("Unknown object type.");
}

cons *
get_list_arg ()
{
    if (NOTP(args))
        error ("Arguments missing.");
    arg1 = ((cons *) args)->car;
    if (NOTP(arg1))
        return nil;
    if (!CONSP(arg1))
        error ("List expected.");
    return arg1;
}

ptr
builtin_car ()
{
    return get_list_arg ()->car;
}

ptr
builtin_cdr ()
{
    return get_list_arg ()->cdr;
}

struct builtin {
    char * name;
    void * func;
} builtins[] = {
    { "?", NULL },
    { "block", NULL },
    { ".", NULL },
    { "car", builtin_car },
    { "cdr", NULL },
    { "rplaca", NULL },
    { "rplacd", NULL },
    { "=", NULL },
    { "read", NULL },
    { "print", NULL },
    { "apply", NULL },
    { "+", NULL },
    { "-", NULL },
    { "*", NULL },
    { "/", NULL },
    { "quote", NULL },
    { "quasiquote", NULL },
    { "quasiquote-splice", NULL },
    { "backquote", NULL },
    { "peek", NULL },
    { "poke", NULL },
    { "sys", NULL },
    { NULL, NULL }
};

int
main (int argc, char * argv[])
{
    ptr env;
    struct builtin * b = builtins;
    symbol * s;

    (void) argc;
    (void) argv;

    term_init ();

    heap = (void *) HEAP_START;
    heap[0] = 0;
    nil = make_symbol ("nil", 3);
    t = make_symbol ("t", 3);
    while (b->name) {
        s = make_symbol (b->name, strlen (b->name));
        s->type |= TYPE_BUILTIN;
        s->value = b->func;
        b++;
    }

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
