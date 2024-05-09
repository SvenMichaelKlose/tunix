#ifndef __CBM__
#define __CBM__
#endif

#define HEAP_START  0x5000

#include <ingle/cc65-charmap.h>

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include <cbm.h>

#include <term/libterm.h>
#include <lisp/liblisp.h>

struct builtin {
    char * name;
    void * func;
} builtins[] = {
    { "?", NULL },
    { "block", NULL },
    { ".", NULL },
    { "car", NULL },
    { "cdr", NULL },
    { "rplaca", NULL },
    { "rplacd", NULL },
    { "=", NULL },
    { "read", NULL },
    { "print", NULL },
    { "apply", NULL },
    { "+", NULL },
    { "-", NULL },
    { "*", NULL },
    { "/", NULL },
    { "quote", NULL },
    { "quasiquote", NULL },
    { "quasiquote-splice", NULL },
    { "backquote", NULL },
    { "peek", NULL },
    { "poke", NULL },
    { "sys", NULL },
    { NULL, NULL }
};

/*
lispptr
apply (lispptr fun, lispptr args)
{
    bind_args (LISPFUN_ARGS(fun), eval_list (args));
    eval_body (LISPFUN_BODY(fun));
    unbind_args (LISPFUN_ARGS(fun));
}
*/

int
main (int argc, char * argv[])
{
    lispptr env;
    struct builtin * b = builtins;
    symbol * s;
    (void) argc, (void) argv;

    term_init ();
    lisp_init ();

    while (b->name) {
        s = lisp_make_symbol (b->name, strlen (b->name));
        s->type |= TYPE_BUILTIN;
        s->value = b->func;
        b++;
    }

    cbm_open (3, 8, 3, "ENV.LISP");
    // TODO: Error checking (smk).
    cbm_k_chkin (3);
    while (env = lisp_read ()) {
        lisp_print (env);
        term_puts ("\n\r");
    }
    cbm_k_close (3);

    term_puts ("\n\rBye!\n\r");
    while (1); // Gone with TUNIX.
    return 0;
}
