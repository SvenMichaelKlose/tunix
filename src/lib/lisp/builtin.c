#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#endif

#include <ctype.h>
#include <limits.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

// Make objects for built-in procedures.
void FASTCALL
add_builtins (struct builtin * b)
{
    symbol * s;
    for (; b->name; b++) {
        s = make_symbol ((char *) b->name, strlen (b->name));
        s->type = TYPE_BUILTIN;
        s->value = b;
        expand_universe (s);
    }
}

// Copy object name to 'buffer' and zero-terminate it.
void FASTCALL
name_to_buffer (lispptr s)
{
    lisp_len = SYMBOL_LENGTH(s);
    memcpy (buffer, SYMBOL_NAME(s), lisp_len);
    buffer[lisp_len] = 0;
}

// Make expression calling procedure 'arg1' with 'args',
// and prepare eval0() to return.
void FASTCALL
make_call (lispptr args)
{
    x = make_cons (arg1, args);
    unevaluated = true;
    PUSH_TAG(TAG_DONE); // Tell to return from eval0().
}

// Make expression calling procedure 'arg1' with CAR(arg2) as the argument,
// and prepare eval0() to return.
void FASTCALL
make_car_call (void)
{
    make_call (make_cons (CAR(arg2), nil));
}
