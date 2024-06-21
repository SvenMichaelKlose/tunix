#include <ingle/cc65-charmap.h>

#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include <lisp/liblisp.h>
#include <simpleio/libsimpleio.h>

void FASTCALL
error (char code, char * msg)
{
    last_errstr = msg;
    has_error = code;
}

void FASTCALL
internal_error (char * msg)
{
    badef = strcpy (buffer, "Internal. ");
    strcpy (badef, msg);
    error (-1, buffer);
}

void
stack_overflow ()
{
    internal_error ("Stack overflow");
}

void
stack_underflow ()
{
    internal_error ("Stack underflow");
}

void
tagstack_overflow ()
{
    internal_error ("Tag stack overflow");
}

void
tagstack_underflow ()
{
    internal_error ("Tag stack underflow");
}

char * FASTCALL
typename (lispptr * x)
{
    if (CONSP(x))
        return "cons";
    if (SYMBOLP(x))
        return "symbol";
    if (BUILTINP(x))
        return "built-in";
    if (NUMBERP(x))
        return "number";
    return "unknown type";
}

void FASTCALL
err_type (char * type, lispptr x)
{
    char * p;
    p = strcpy (buffer, type);
    p = strcpy (p, " wanted. Got ");
    p = strcpy (p, typename (x));
    error (ERROR_TYPE, buffer);
}

void FASTCALL
bi_tcheck (lispptr x, uchar type)
{
    (void) x, (void) type;

    switch (type) {
    case 'x': // anything
        break;

    case 'n': // number
        if (!NUMBERP(x))
            err_type ("number", x);
        break;

    case 's': // symbol
        if (!SYMBOLP(x))
            err_type ("symbol", x);
        break;

    case 'c': // cons
        if (!CONSP(x))
            err_type ("cons", x);
        break;

    case 'l': // list (cons or nil)
        if (!LISTP(x))
            err_type ("list", x);
        break;

    case 'f': // function
        if (!LISTP(x) && !BUILTINP(x))
            err_type ("Function", x);
        break;

#ifndef NDEBUG
    default:
        setout (STDERR);
        out (type);
        outs ("': unknown typedef");
        while (1);
#endif
    }
}
