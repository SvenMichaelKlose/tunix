#include <ingle/cc65-charmap.h>

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include <lisp/liblisp.h>

void
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
