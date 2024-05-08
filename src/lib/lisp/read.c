#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include <term/libterm.h>

#include "liblisp.h"
#include "io.h"

char token[256];

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
            c = lisp_read ();
        else {
            putback ();
            c = lisp_make_cons (lisp_read (), nil);
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
    for (p = token; !eof () && isdigit (in ()); p++)
        *p = ch ();
    *p = 0;
    putback ();
    return lisp_make_number (atoi (token));
}

bool __fastcall__
our_isalpha (char c)
{
    return !isspace (c) && c != '(' && c != ')';
}

lispptr
read_symbol ()
{
    char * p;
    for (p = token; !eof () && our_isalpha (in ()); p++)
        *p = ch ();
    putback ();
    return lisp_make_symbol (token, p - token);
}

lispptr
lisp_read ()
{
    skip_spaces ();
    if (eof ())
        return NULL;
    in ();
    putback ();
    if (ch () == '(')
        return read_list ();
    if (isdigit (ch ()))
        return read_number ();
    return read_symbol ();
}
