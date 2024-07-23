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

bool do_highlight;

void print0 (lispptr);

void
space (void)
{
    if (last_out != '('
        && last_out != ')'
        && last_out != '\''
        && last_out > ' ')
        out (' ');
}

void FASTCALL
print_short (char * m, cons * c)
{
    outs (m);
    last_out = ' '; // Avoid output padding.
    print0 (LIST_CAR(LIST_CDR(c)));
}

void FASTCALL
print_list (cons * c)
{
    bool first = true;

#ifdef PRINT_SHORT_QUOTES
    if (CDR(c)) {
        tmp = CAR(c);
        if (tmp ==  quote) {
            print_short ("'", c);
            return;
        }
        if (tmp ==  quasiquote) {
            print_short ("$", c);
            return;
        }
        if (tmp ==  unquote) {
            print_short (",", c);
            return;
        }
        if (tmp ==  unquote_spliced) {
            print_short (",@", c);
            return;
        }
    }
#endif

    out ('(');
    while (c) {
        if (!first)
            out (' ');
        else
            first = false;
        print0 (c->car);
        if (c->cdr && !CONSP(c->cdr)) {
            outs (" . ");
            print0 (c->cdr);
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
    outn (n->value);
}

bool FASTCALL
needs_quotes (symbol * s)
{
    char * p = SYMBOL_NAME(s);
    char len = SYMBOL_LENGTH(s);
    char c;
    for (; len--; p++) {
        c = *p;
        if (c == '"' || c == ' ' || c == '(' || c == ')')
            return true;
    }
    return false;
}

void FASTCALL
print_quoted_string (symbol * s)
{
    char * p = SYMBOL_NAME(s);
    char len = SYMBOL_LENGTH(s);
    char c;
    out ('"');
    for (; len--; p++) {
        c = *p;
        if (c == '"')
            out ('\\');
        out (c);
    }
    out ('"');
}

void FASTCALL
print_named (symbol * s)
{
    space ();
#ifndef NO_PRINT_ANONYMOUS
    if (!SYMBOL_LENGTH(s)) {
        outs ("<?>");
        return;
    }
#endif
    if (needs_quotes (s)) {
        print_quoted_string (s);
        return;
    }
    outsn (SYMBOL_NAME(s), SYMBOL_LENGTH(s));
}

void FASTCALL
print_highlighted (lispptr x, bool is_before)
{
    if (do_highlight && current_expr == x)
        outs (is_before ? ">>>" : "<<<");
}

void FASTCALL
print0 (lispptr x)
{
    uchar type;

    print_highlighted (x, true);
    if (!x) {
        outs ("nil");
        goto done;
    }
    type = TYPEBITS(x);
    if (type & TYPE_CONS)
        print_list ((cons *) x);
    else if (type & TYPE_NUMBER)
        print_number ((number *) x);
    else if (_NAMEDP(x))
        print_named ((symbol *) x);
#ifndef NDEBUG
    else
        internal_error ("Unknown object type.");
#endif
done:
    print_highlighted (x, false);
}

lispptr FASTCALL
print (lispptr x)
{
    print0 (x);
    return x;
}

#ifndef NDEBUG
lispptr FASTCALL
dprint (lispptr x)
{
    print0 (x);
    terpri ();
    return x;
}
#endif
