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
lispptr highlighted;

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

#define HIGHLIGHT_CAR       false
#define HIGHLIGHT_CDR       true
#define HIGHLIGHT_BEFORE    false
#define HIGHLIGHT_AFTER     true

void FASTCALL
print_highlighting (lispptr x, bool which, bool when)
{
    if (!do_highlight)
        return;
    if ((which == HIGHLIGHT_CAR && LIST_CAR(highlighted) == x)
        || LIST_CDR(highlighted) == x)
        outs (when == HIGHLIGHT_BEFORE ? ">>>" : "<<<");
}

// Print abbreviation.
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
        if (tmp == quote) {
            print_short ("'", c);
            return;
        }
        if (tmp == quasiquote) {
            print_short ("$", c);
            return;
        }
        if (tmp == unquote) {
            print_short (",", c);
            return;
        }
        if (tmp == unquote_spliced) {
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
        print_highlighting (c, HIGHLIGHT_CAR, HIGHLIGHT_BEFORE);
        print0 (c->car);
        print_highlighting (c, HIGHLIGHT_CAR, HIGHLIGHT_AFTER);
        if (c->cdr && !CONSP(c->cdr)) {
            outs (" . ");
            print_highlighting (c, HIGHLIGHT_CDR, HIGHLIGHT_BEFORE);
            print0 (c->cdr);
            print_highlighting (c, HIGHLIGHT_CDR, HIGHLIGHT_AFTER);
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
print0 (lispptr x)
{
    uchar type;

    if (!x) {
        outs ("nil");
        return;
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
        CHKPTR(x);
#endif
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
