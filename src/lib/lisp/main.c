#ifndef __CBM__
#define __CBM__
#endif

#include <ingle/cc65-charmap.h>

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include <cbm.h>

#include <term/libterm.h>

#include "liblisp.h"

//#pragma bss-name (push, "ZEROPAGE")
char *    heap;
symbol *  s;
char      c;
char      do_putback;
lispptr       nil;
lispptr       t;
lispptr       args;
lispptr       arg1;
lispptr       arg2;
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

char token[256];

lispptr __fastcall__
alloc (uchar size, uchar type)
{
    char * r = heap;
    heap[0] = size;
    heap[1] = type;
    heap += size;
    heap[0] = 0;    // Mark end of heap.
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
    number * n = alloc (sizeof (number),
                        TYPE_NUMBER);
    n->value = x;
    return n;
}

void * __fastcall__
lookup_symbol (char * str, uchar len)
{
    symbol * s = alloc (sizeof (symbol),
                        TYPE_SYMBOL);

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
    symbol * s = lookup_symbol (str, len);
    if (s)
        return s;

    s = alloc (sizeof (symbol) + len,
               TYPE_SYMBOL);
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
            while (!eof ()
                   && in () >= ' ');
            while (!eof ()
                   && in () < ' ');
            putback ();
            continue;
        }
        if (!isspace (c)) {
            putback ();
            return;
        }
    }
}

lispptr read (void);

lispptr
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

lispptr
read_number ()
{
    char * p = token;

    while (!eof ()
           && isdigit (in ()))
        *p++ = c;
    *p = 0;
    putback ();
    return make_number (atoi (token));
}

bool __fastcall__
our_isalpha (char c)
{
    return !isspace (c)
           && c != '('
           && c != ')';
}

lispptr
read_symbol ()
{
    char * p = token;

    while (!eof ()
           && our_isalpha (in ()))
        *p++ = c;
    putback ();
    return make_symbol (token, p - token);
}

lispptr
lisp_read ()
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

void print (lispptr x);

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
lisp_print (lispptr x)
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

void
lisp_init ()
{
    nil = lisp_make_symbol ("nil", 3);
    t   = lisp_make_symbol ("t", 3);
}
