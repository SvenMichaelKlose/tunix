#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#endif

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <setjmp.h>
#ifdef TARGET_UNIX
#include <signal.h> // For HOST_DEBUGGER().
#endif

#include <simpleio/libsimpleio.h>

#include "liblisp.h"

#ifdef __CC65__
#pragma code-name (push, "CODE_READ")
#endif

bool FASTCALL
our_isalpha (char c)
{
    return !isspace (c) && !isdigit (c) &&
           c != '(' && c != ')' && c != ';';
}

#ifndef NAIVE

lispptr
missing_closing_paren (void)
{
    error (ERROR_PAREN_MISSING, "Missing ')'");
    return t;
}

lispptr
is_unexpected_eol (void)
{
    return eof () ? missing_closing_paren () : nil;
}

#endif // #ifndef NAIVE

void
skip_comments_and_spaces ()
{
    if (skip_spaces ())
        return;

    // Skip comment.
    if (in () == ';')  {
        // Skip until control chars.
        while (in () >= ' ')
            if (eof ())
                return;
        if (skip_spaces ())
            return;
    }
    putback ();
}

lispptr
read_list (void)
{
    cons * c;
    cons * start = nil;
    cons * last  = nil;

    while (1) {
        skip_comments_and_spaces ();

#ifndef NAIVE
        if (is_unexpected_eol ())
            return nil;
#endif

        // End of list.
        if (in () == ')')
            return start;
#ifndef NAIVE
        if (is_unexpected_eol ())
            return nil;
#endif
        putback ();

        PUSH(start);
        PUSH(last);

        // Dotted pair?
        if (in () == '.' && NOT_NIL(start)) {
            // Read dotted pair's CDR.
            c = read_expr ();

#ifndef NAIVE
            // Ensure end of list.
            skip_comments_and_spaces ();
            if (in () != ')' || eof ())
                return missing_closing_paren ();
            putback (); // Keep for regular end-of-list detection.
#endif
        } else {
            // Read next element.
            putback ();
            c = make_cons (read_expr (), nil);
        }

        POP(last);
        POP(start);

#ifndef NAIVE
        if (error_code)
            return nil;
#endif

        // Append element to last.
        if (NOT_NIL(last))
            last->cdr = c;
        else
            start = c;
        last = c;
    }

    /* NOTREACHED */
    return nil;
}

lispptr
read_string (void)
{
    char * p;
    for (p = buffer; p != &buffer[MAX_SYMBOL] && in () != '"'; p++) {
        *p = (lastin () == '\\') ? in () : lastin ();
#ifndef NAIVE
        if (eof ()) {
            error (ERROR_QUOTE_MISSING, "No '\"'");
            return nil;
        }
#endif
    }
    return make_symbol (buffer, p - buffer);
}

lispptr FASTCALL
read_quoted (lispptr which)
{
    return make_cons (which, make_cons (read_expr (), nil));
}

#ifndef NO_QUASIQUOTE

lispptr
read_unquoted (void)
{
    if (in () == '@')
        return read_quoted (unquote_spliced);
    putback ();
    return read_quoted (unquote);
}

#endif // #ifndef NO_QUASIQUOTE

lispptr
read_symbol_or_number (void)
{
    bool is_number = false;
    char * p;
    lispobj_size_t len = 0;

    // Read char by char...
    for (p = buffer; in (); p++) {
        if (eof ())
            break;

#ifndef NAIVE
        // Check if buffer is full.
        if (len == MAX_SYMBOL) {
            error (ERROR_SYM_TOO_LONG, "Sym len");
            return nil;
        }
#endif

        // Determine type or end.
        if (our_isalpha (lastin ())) {
            // Valid symbol char but not a digit.
            // Break if we determined that it's a number.
            if (is_number)
                break;
        } else if (isdigit (lastin ())) {
            // Got a digit.  Make it a number if it's the
            // first char.  Also if it's preceded by a minus
            // as the first char.
            if (!is_number)
                if (!len || (len == 1 && *buffer == '-'))
                    is_number = true;
        } else
            // Break on anything else.
            break;

        // Add char to buffer.
        *p = lastin ();
        len++;

    }
    putback ();

    // Make number.
    if (is_number) {
        *p = 0; // Zero-terminate for atoi().
        if (buffer[0] == '-')
            return make_number (-atoi (&buffer[1]));
        return make_number (atoi (buffer));
    }

    // Make symbol.
    if (len == 3 && !memcmp (buffer, "nil", 3))
        return nil;
    return make_symbol (buffer, len);
}

lispptr
read_expr ()
{
    skip_comments_and_spaces ();
    if (eof ())
        return nil;

    switch (in ()) {
    case '(':
        return read_list ();
    case '"':
        return read_string ();
    case '\'':
        return read_quoted (quote);
#ifndef NO_QUASIQUOTE
    case '$':
        return read_quoted (quasiquote);
    case ',':
        return read_unquoted ();
#endif
    case '\\':
        return make_number (in ());
#ifndef NAIVE
    case ')':
        error (ERROR_STALE_PAREN, "Stale ')'");
        return nil;
#endif
    }
    putback ();
    return read_symbol_or_number ();
}
