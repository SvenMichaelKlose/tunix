#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#endif

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern lispptr x;
extern lispptr args;
extern lispptr arg1;
extern lispptr arg2c;
extern lispptr arg2;
extern lispptr value;
extern lispptr tmp;
#ifdef __CC65__
#pragma zpsym ("x")
#pragma zpsym ("args")
#pragma zpsym ("arg1")
#pragma zpsym ("arg2c")
#pragma zpsym ("arg2")
#pragma zpsym ("value")
#pragma zpsym ("tmp")
#pragma bss-name (pop)
#endif

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

void FASTCALL
name_to_buffer (lispptr s)
{
    uchar len;  // TODO: tmpc
    len = SYMBOL_LENGTH(s);
    memcpy (buffer, SYMBOL_NAME(s), len);
    buffer[len] = 0;
}

void FASTCALL
make_call (lispptr args)
{
    x = make_cons (arg1, args);
    unevaluated = true;
    PUSH_TAG(TAG_DONE); // Tell to return from eval0().
}

void FASTCALL
make_car_call (void)
{
    make_call (make_cons (CAR(arg2), nil));
}
