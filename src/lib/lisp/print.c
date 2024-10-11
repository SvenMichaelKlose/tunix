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
#include <setjmp.h>
#if defined(TARGET_UNIX) && !defined(NDEBUG)
#include <stdio.h>
#endif

#include <simpleio/libsimpleio.h>

#include "liblisp.h"

#ifndef NO_HIGHLIGHTING
bool do_highlight;
lispptr highlighted;
#endif
lispptr print_tmp;

#ifdef __CC65__
#pragma code-name (push, "CODE_PRINT")
#endif

void print0 (lispptr);

void
space (void)
{
    char c = lastout ();
    if (c != '(' && c != ')' && c != '\'' && c > ' ')
        out (' ');
}

#define HIGHLIGHT_CAR       false
#define HIGHLIGHT_CDR       true
#define HIGHLIGHT_BEFORE    false
#define HIGHLIGHT_AFTER     true

#ifndef NO_HIGHLIGHTING

void FASTCALL
print_highlighted (lispptr x, bool when)
{
    if (do_highlight && highlighted == x)
#if defined(TARGET_C128) || defined(TARGET_C16) || defined(TARGET_C64) || defined(TARGET_PET) || defined(TARGET_PLUS4) || defined(TARGET_VIC20)
        out (when == HIGHLIGHT_BEFORE ? 18 : 146);
#elif TARGET_UNIX
        outs (when == HIGHLIGHT_BEFORE ? "\033[7m\000" : "\033[27m\000");
#else
        outs (when == HIGHLIGHT_BEFORE ? ">>>" : "<<<");
#endif
}

#endif // #ifndef NO_HIGHLIGHTING

// Print abbreviation.
void FASTCALL
print_short (char * m, cons * c)
{
    outs (m);
    print0 (LIST_CAR(LIST_CDR(c)));
}

void FASTCALL
print_list (cons * c)
{
    bool first = true;

#ifdef PRINT_SHORT_QUOTES
    if (CDR(c)) {
        tmpstr = NULL;
        print_tmp = CAR(c);
        if (print_tmp == quote)
            tmpstr = "'";
#ifndef NO_QUASIQUOTE
        else if (print_tmp == quasiquote)
            tmpstr = "$";
        else if (print_tmp == unquote)
            tmpstr = ",";
        else if (print_tmp == unquote_spliced)
            tmpstr = ",@";
#endif
        print_tmp = nil;
        if (tmpstr) {
            print_short (tmpstr, c);
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
#ifndef NO_HIGHLIGHTING
        print_highlighted (c, HIGHLIGHT_BEFORE);
#endif
        print0 (c->car);
#ifndef NO_HIGHLIGHTING
        print_highlighted (c, HIGHLIGHT_AFTER);
#endif
        print_tmp = CDR(c);
        if (NOT_NIL(print_tmp) && !CONSP(print_tmp)) {
            outs (" . ");
#ifndef NO_HIGHLIGHTING
            print_highlighted (c, HIGHLIGHT_BEFORE);
#endif
            print0 (print_tmp);
#ifndef NO_HIGHLIGHTING
            print_highlighted (c, HIGHLIGHT_AFTER);
#endif
            break;
        }
        c = CDR(c);
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
        outs ("\"\"");
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

    if (NOT(x)) {
        space ();
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
    // Error.  Have check_lispptr() issue it.
    else
        CHKPTR(x);
#endif
}

lispptr FASTCALL
print (lispptr x)
{
    print0 (x);
    if (CONSP(x))
        terpri ();
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
