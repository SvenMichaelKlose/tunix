#ifdef __CC65__
#ifndef __CBM__
#define __CBM__
#endif

#include <ingle/cc65-charmap.h>
#include <cbm.h>
#endif

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>

#include <simpleio/libsimpleio.h>

#include "liblisp.h"

extern void error (char * msg);

void print (lispptr);

void
space (void)
{
    if (last_out != '('
        && last_out != ')'
        && last_out > ' ')
        out (' ');
}

void FASTCALL
print_list (cons * c)
{
    bool first = true;

    if (CAR(c) == quote && CDR(c)) {
        out ('\'');
        print (CAR(CDR(c)));
        return;
    }

    out ('(');
    while (c) {
        if (!first)
            out (' ');
        else
            first = false;
        print (c->car);
        if (c->cdr && !CONSP(c->cdr)) {
            outs (" . ");
            print (c->cdr);
            break;
        }
        c = c->cdr;
    }
    out (')');
}

void FASTCALL
print_number (number * n)
{
    space ();
    out_number (n->value);
}

void FASTCALL
print_symbol (symbol * s)
{
    space ();
    outsn (SYMBOL_NAME(s), SYMBOL_LENGTH(s));
}

void
print (lispptr x)
{
    uchar type;

    if (!x) {
        outs ("nil");
        return;
    }
    type = TYPE(x);
    if (type == TYPE_CONS)
        print_list ((cons *) x);
    else if (type == TYPE_NUMBER)
        print_number ((number *) x);
    else if (PTRTYPE(x) & TYPE_NAMED)
        print_symbol ((symbol *) x);
    else
        error ("Unknown object type.");
}

lispptr FASTCALL
lisp_print (lispptr x)
{
    print (x);
    out ('\n');
    return x;
}
