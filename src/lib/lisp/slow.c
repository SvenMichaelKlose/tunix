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
    return x && TYPE(x) == TYPE_CONS;
}

bool FASTCALL
lisp_atom (lispptr x)
{
    return !x || TYPE(x) != TYPE_CONS;
}

bool FASTCALL
lisp_listp (lispptr x)
{
    return !x || TYPE(x) == TYPE_CONS;
}

bool FASTCALL
lisp_numberp (lispptr x)
{
    return x && TYPE(x) == TYPE_NUMBER;
}

bool FASTCALL
lisp_symbolp (lispptr x)
{
    return !x || TYPE(x) == TYPE_SYMBOL;
}

bool FASTCALL
lisp_builtinp (lispptr x)
{
    return x && TYPE(x) == TYPE_BUILTIN;
}

bool FASTCALL
lisp_specialp (lispptr x)
{
    return x && PTRTYPE(x) & TYPE_SPECIAL;
}
