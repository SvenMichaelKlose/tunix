#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#include <cbm.h>
#endif

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

lispptr FASTCALL
lisp_car (lispptr x)
{
    return CONS(x)->car;
}

lispptr FASTCALL
lisp_cdr (lispptr x)
{
    return CONS(x)->cdr;
}

bool FASTCALL
lisp_consp (lispptr x)
{
    return _CONSP(x);
}

bool FASTCALL
lisp_atom (lispptr x)
{
    return _ATOM(x);
}

bool FASTCALL
lisp_listp (lispptr x)
{
    return _LISTP(x);
}

bool FASTCALL
lisp_numberp (lispptr x)
{
    return _NUMBERP(x);
}

bool FASTCALL
lisp_symbolp (lispptr x)
{
    return _SYMBOLP(x);
}

bool FASTCALL
lisp_builtinp (lispptr x)
{
    return _BUILTINP(x);
}

bool FASTCALL
lisp_specialp (lispptr x)
{
    return _SPECIALP(x);
}

void FASTCALL
pushgc (lispptr x)
{
    STACK_CHECK_OVERFLOW();
    stack -= sizeof (lispptr);
    *(lispptr *) stack = x;
}

lispptr
popgc ()
{
    STACK_CHECK_UNDERFLOW();
    tmp2 = *(lispptr *) stack;
    stack += sizeof (lispptr);
    return tmp2;
}

void FASTCALL
pushtag (char x)
{
    TAGSTACK_CHECK_OVERFLOW();
    *--tagstack = x;
}

char
poptag ()
{
    TAGSTACK_CHECK_UNDERFLOW();
    return *tagstack++;
}

void FASTCALL
pushtagw (lispptr x)
{
    TAGSTACK_CHECK_OVERFLOW();
    tagstack -= sizeof (lispptr);
    *(lispptr *) tagstack = x;
}

lispptr
poptagw ()
{
    TAGSTACK_CHECK_UNDERFLOW();
    tmp2 = *(lispptr *) tagstack;
    tagstack += sizeof (lispptr);
    return tmp2;
}
