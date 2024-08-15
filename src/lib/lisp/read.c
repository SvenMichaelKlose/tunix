#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#endif

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <setjmp.h>

#include <simpleio/libsimpleio.h>

#include "liblisp.h"

#ifdef __CC65__
#pragma code-name (push, "CODE_READ")
#endif

bool FASTCALL
our_isalpha (char c)
{
    return !isspace (c) && c != '(' && c != ')' && c != ';';
}

#ifndef NAIVE
void
missing_closing_paren (void)
{
    error (ERROR_NO_PAREN, "No ')'");
}
#endif

lispptr
read_list (void)
{
    cons * c;
    cons * start = NULL;
    cons * last = NULL;

    while (1) {
        skip_spaces ();
#ifndef NAIVE
        if (eof ())
            missing_closing_paren ();
#endif
        if (in () == ')')
            return start;
#ifndef NAIVE
        if (eof ())
            missing_closing_paren ();
#endif
        putback ();

        PUSH(start);
        PUSH(last);
        if (in () == '.')
            c = read ();
        else {
            putback ();
            c = make_cons (read (), nil);
        }
        POP(last);
        POP(start);
        if (last)
            last->cdr = c;
        else
            start = c;
        last = c;
    }

    /* NOTREACHED */
    return nil;
}

lispptr
read_number (void)
{
    char * p = buffer;
    for (p = buffer; !eof () && isdigit (in ()); p++)
        *p = last_in;
    *p = 0;
    putback ();
    return make_number (atoi (buffer));
}

lispptr
read_symbol (void)
{
    char * p;
    unsigned char len;
    for (p = buffer; !eof () && our_isalpha (in ()); p++)
        *p = last_in;
    putback ();
    len = p - buffer;
    if (len == 3 && !memcmp (buffer, "nil", 3))
        return nil;
    return make_symbol (buffer, p - buffer);
}

lispptr
read_string (void)
{
    char * p;
    for (p = buffer;
         p != &buffer[MAX_SYMBOL] && !eof () && in () != '"';
         p++)
        *p = (last_in == '\\') ? in () : last_in;
    return make_symbol (buffer, p - buffer);
}

lispptr FASTCALL
read_quoted (lispptr which)
{
    return make_cons (which, make_cons (read (), nil));
}

lispptr
read_unquoted (void)
{
    if (in () == '@')
        return read_quoted (unquote_spliced);
    putback ();
    return read_quoted (unquote);
}

lispptr
read ()
{
    skip_spaces ();
    in ();
    if (eof ())
        return nil;
    // Skip one-line comment.
    if (last_in == ';')  {
        while (in () >= ' ')
            if (eof ())
                return nil; // TODO: EOF object to set with eof(s).
        while (in () < ' ')
            if (eof ())
                return nil;
        putback ();
    }
    switch (last_in) {
    case '(':
        return read_list ();
    case '"':
        return read_string ();
    case '\'':
        return read_quoted (quote);
    case '$':
        return read_quoted (quasiquote);
    case ',':
        return read_unquoted ();
#ifndef NAIVE
    case ')':
        error (ERROR_STALE_PAREN, "Stale ')'");
        return nil;
#endif
    }
    putback ();
    // TODO: Negative numbers.
    if (isdigit (last_in))
        return read_number ();
    return read_symbol ();
}
