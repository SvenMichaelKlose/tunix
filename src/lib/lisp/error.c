#include <ingle/cc65-charmap.h>

#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#ifndef __CC65__
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
#endif
    exit (ERROR_INTERNAL);
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
err_type (char * type, lispptr x)
{
    char * p;
    p = stpcpy (buffer, typename (x));
    p = stpcpy (p, " instead of ");
    strcpy (p, type);
    error (ERROR_TYPE, buffer);
}

// Type check object and issue error if it fails.
void FASTCALL
bi_tcheck (lispptr x, uchar type)
{
    (void) x, (void) type;

    switch (type) {
    case 'x':
        break;

    case 'n':
        if (!NUMBERP(x))
            err_type ("number", x);
        break;

    case 's':
        if (!SYMBOLP(x))
            err_type ("symbol", x);
        break;

    case 'c':
        if (!CONSP(x))
            err_type ("cons", x);
        break;

    case 'l':
        if (!LISTP(x))
            err_type ("list", x);
        break;

    case 'f':
        if (!LISTP(x) && !BUILTINP(x))
            err_type ("function", x);
        break;

#ifndef NDEBUG
    default:
        internal_error ("ill typedef");
#endif
    }
}

// Issue error if GC stack and tag stack pointers deviate from arguments.
void FASTCALL
check_stacks (char * old_stack, char * old_tagstack)
{
    if (old_stack != stack)
        internal_error ("stack");
    if (old_tagstack != tagstack)
        internal_error ("tagstack");
}

// Print error info.
void
print_error_info ()
{
    if (error_code) {
        outs ("Error #");
        outn (error_code);
        outs (": ");
        if (last_errstr)
            outs (last_errstr);
        terpri ();
    }
    fresh_line ();
    outs ("In");
    do_highlight = true;
    if (current_function) {
        outs (" function ");
        if (SYMBOLP(current_function)) {
            print (current_function);
            out (' ');
            print (FUNARGS(current_function));
            outs (": ");
            terpri ();
            print (FUNBODY(current_function));
        } else {
            outs (": ");
            print (current_function);
        }
    } else {
        outs (": ");
        print (current_toplevel);
    }
    do_highlight = false;
    terpri ();
}

#ifndef NO_ONERROR
void
init_onerror ()
{
    onerror_sym = make_symbol ("onerror", 7);
    expand_universe (onerror_sym);
}
#endif // #ifndef NO_ONERROR

#endif // #ifndef NAIVE
