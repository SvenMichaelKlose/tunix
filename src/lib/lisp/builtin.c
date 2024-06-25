#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#endif

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

void FASTCALL
add_builtins (struct builtin * b)
{
    symbol * s;
    for (; b->name; b++) {
        s = make_symbol (b->name, strlen (b->name));
        s->type = TYPE_BUILTIN;
        s->value = b;
        expand_universe (s);
    }
}
