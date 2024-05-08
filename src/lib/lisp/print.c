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

#include <term/libterm.h>

#include "io.h"
#include "liblisp.h"

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
        lisp_print (c->car);
        if (c->cdr != nil && !CONSP(c->cdr)) {
            outs (" . ");
            lisp_print (c->cdr);
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
