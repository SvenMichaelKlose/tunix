#ifdef __CC65__
#include <ingle/cc65-charmap.h>
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

// Issue error, with code and message.
// Causes call of ONERROR handler or debugger.
void FASTCALL
error (char code, char * msg)
{
    last_errstr = msg;
    error_code = code;
}

// Print internal error message and exit.
void FASTCALL
internal_error (char * msg)
{
    outs (msg); terpri ();
#ifdef TARGET_UNIX
    raise (SIGTRAP);
    exit (EXIT_FAILURE);
#else
    while (1);
#endif
}

// Print internal error message and exit.
void FASTCALL
internal_error_ptr (void * p, char * msg)
{
    outhw ((size_t) p);
    out (' ');
    internal_error (msg);
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
    p = stpcpy (buffer, "got ");
    p = stpcpy (p, typename (x));
    p = stpcpy (p, " instead of ");
    strcpy (p, type);
    error (code, buffer);
}

// Type check object and issue error if it fails.
// TODO: Do not pass on error code via argument lists.
void FASTCALL
bi_tcheck (lispptr x, uchar type, char code)
{
    (void) x, (void) type;

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

// Issue error if GC stack and tag stack pointers deviate from arguments.
void FASTCALL
check_stacks (char * old_stack, char * old_tagstack)
{
    if (old_stack != stack)
        internal_error_ptr (stack, "stack");
    if (old_tagstack != tagstack)
        internal_error_ptr (tagstack, "tagstack");
}

#ifndef NO_ONERROR
#ifdef TARGET_VIC20
#pragma code-name (push, "LISPSTART")
#endif
void
init_onerror ()
{
    onerror_sym = make_symbol ("onerror", 7);
    expand_universe (onerror_sym);
}
#ifdef TARGET_VIC20
#pragma code-name (pop)
#endif
#endif // #ifndef NO_ONERROR

#endif // #ifndef NAIVE
