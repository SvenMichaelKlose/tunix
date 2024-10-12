#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#pragma inline-stdfuncs (off)
#pragma allow-eager-inline (off)
#endif

#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <setjmp.h>
#ifdef TARGET_UNIX
#include <signal.h>
#endif

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

#ifndef NAIVE

char * last_errstr;

#ifdef __CC65__
#pragma code-name ("CODE_ERROR")
#endif

// Issue error, with code and message.
// Causes call of ONERROR handler or debugger.
void FASTCALL
error (char code, char * msg)
{
    last_errstr = msg;
    error_code = code;
#ifdef HOST_DEBUGGER_ON_ERROR
    HOST_DEBUGGER();
#endif
}

void FASTCALL
error_argname (lispptr x)
{
    error_info = x;
    error (ERROR_ARGNAME_TYPE, "Arg not a symbol");
}

lispptr FASTCALL
error_cons_expected (lispptr x)
{
    error_info = x;
    error (ERROR_TYPE, "not a cons");
    return nil;
}

// Print internal error message and exit.
void FASTCALL
internal_error (char * msg)
{
    (void) con_reset ();
    outs ("INTERNAL: ");
    outs (msg);
    terpri ();
#ifdef TARGET_UNIX
    raise (SIGTRAP);
    exit (EXIT_FAILURE);
#elif TARGET_SIM6502
    exit (EXIT_FAILURE);
#else
    while (1);
#endif
}

// Print internal error message and exit.
void FASTCALL
internal_error_ptr (void * p, char * msg)
{
    (void) con_reset ();
    outhw ((size_t) p);
    out (' ');
    internal_error (msg);
}

void
stack_overflow ()
{
    internal_error ("GC overflow");
}

void
stack_underflow ()
{
    internal_error ("GC underflow");
}

void
tagstack_overflow ()
{
    internal_error ("Tag overflow");
}

void
tagstack_underflow ()
{
    internal_error ("Tag underflow");
}

void
error_set_ccons_cdr ()
{
    internal_error ("ccons CDR");
}

// Return type name of object.
char * FASTCALL
typename (lispptr * x)
{
    if (CONSP(x))
        return "cons";
    if (SYMBOLP(x))
        return "symbol";
    if (BUILTINP(x))
        return "builtin";
#ifndef NDEBUG
    if (NUMBERP(x))
        return "number";
    return NULL;
#else
    return "number";
#endif
}

// Issue type error.
void FASTCALL
err_type (char * type, lispptr x, char code)
{
    char * p;
    p = stpcpy (buffer, "Got ");
    p = stpcpy (p, typename (x));
    p = stpcpy (p, ", not ");
    strcpy (p, type);
    error_info = make_symbol (type, strlen (type));
    error (code, buffer);
}

#ifndef NAIVE

// Type check object and issue error if it fails.
// TODO: Do not pass on error code via argument lists.
void FASTCALL
bi_tcheck (lispptr x, uchar type, char code)
{
    (void) type;
    error_info = x;

    switch (type) {
    case 'x':
        break;

    case 'n':
        if (!NUMBERP(x))
            err_type ("number", x, code);
        break;

    case 's':
        if (!SYMBOLP(x))
            err_type ("symbol", x, code);
        break;

    case 'S':
        if (NOT_NIL(x) && !_NAMEDP(x))
            err_type ("string", x, code);
        break;

    case 'c':
        if (!CONSP(x))
            err_type ("cons", x, code);
        break;

    case 'l':
        if (!LISTP(x))
            err_type ("list", x, code);
        break;

    case 'f':
        if (!LISTP(x) && !BUILTINP(x))
            err_type ("function", x, code);
        break;

#ifndef NDEBUG
    default:
        internal_error_ptr (x, "ill typedef");
#endif
    }
}

#endif // #ifndef NAIVE

#ifndef NDEBUG

// Issue error if GC stack and tag stack pointers deviate from arguments.
void FASTCALL
check_stacks (char * old_stack, char * old_tagstack)
{
    if (old_stack != stack)
        internal_error_ptr (stack, "stack misaligned");
    if (old_tagstack != tagstack)
        internal_error_ptr (tagstack, "tagstack misaligned");
}

#endif // #ifndef NDEBUG

#endif // #ifndef NAIVE
