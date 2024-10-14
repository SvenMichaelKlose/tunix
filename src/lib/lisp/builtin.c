#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#endif

#include <ctype.h>
#include <limits.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <setjmp.h>

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

#ifdef __CC65__
#pragma code-name ("CODE_BUILTIN")
#endif

// Copy object name to 'buffer' and zero-terminate it.
void FASTCALL
name_to_buffer (lispptr s)
{
    lispobj_size_t l = SYMBOL_LENGTH(s);
    memcpy (buffer, SYMBOL_NAME(s), l);
    buffer[l] = 0;
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

#ifdef __CC65__
#pragma code-name ("CODE_INIT")
#pragma inline-stdfuncs (off)
#pragma allow-eager-inline (off)
#pragma codesize (10)
#endif

// Make objects for built-in procedures.
void FASTCALL
add_builtins (const struct builtin * b)
{
    symbol * s;
    for (; b->name; b++) {
        s = make_symbol ((char *) b->name, strlen (b->name));
        s->type = TYPE_BUILTIN;
        s->value = (void *) b;
        expand_universe (s);
    }
}
