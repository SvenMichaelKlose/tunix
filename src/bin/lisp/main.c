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
    { "and", NULL },
    { "or", NULL },
    { "block", NULL },
    { "eq", NULL },
    { "atom", NULL },
    { "not", NULL },
    { ".", NULL },
    { "car", NULL },
    { "cdr", NULL },
    { "rplaca", NULL },
    { "rplacd", NULL },
    { "set", NULL },
    { "==", NULL },
    { ">", NULL },
    { "<", NULL },
    { ">=", NULL },
    { "<=", NULL },
    { "read", NULL },
    { "print", NULL },
    { "apply", NULL },
    { "+", NULL },
    { "++", NULL },
    { "-", NULL },
    { "--", NULL },
    { "*", NULL },
    { "/", NULL },
    { "%", NULL },
    { "quote", NULL },
    { "quasiquote", NULL },
    { "quasiquote-splice", NULL },
    { "backquote", NULL },
    { "peek", NULL },
    { "poke", NULL },
    { "sys", NULL },
    { "bit-and", NULL },
    { "bit-or", NULL },
    { "bit-xor", NULL },
    { NULL, NULL }
};

/*
lispptr
apply (lispptr fun, lispptr args)
{
    lispptr argdef = LISPFUN_ARGS(fun);
    lispptr ad;
    lispptr av;
    lispptr name;
    unsigned stsize = 0;

    // Push argument symbol values onto the stack.
    for (ad = argdef, av = args;
         ad != nil && av != nil;
         ad = CDR(ad), av = CDR(av)) {
        stsize++;

        // Rest of argument list. (consing)
        if (LISPATOM(ad)) {
            LISPPUSH(ad);
            LISPPUSH(SYMBOL_VALUE(ad));
            SETQ_SYMBOL_VALUE(ad, eval_list (av));
            break;
        }

        name = CAR(ad);
        LISPPUSH(name);
        LISPPUSH(SYMBOL_VALUE(name));
        SETQ_SYMBOL_VALUE(name, eval (CAR(av)));
    }

    LISPPUSH(stsize);
    eval_body (LISPFUN_BODY(fun));
    LISPPOP(stsize);

    // Pop argument symbol values from the stack.
    while (stsize--) {
        LISPPOP(name);
        LISPPOP(SYMBOL_VALUE(name));
    }
}
*/

int
main (int argc, char * argv[])
{
    lispptr env;
    struct builtin * b = builtins;
    symbol * s;
    unsigned i;
    (void) argc, (void) argv;

    term_init ();
    lisp_init ();

    while (b->name) {
        s = lisp_make_symbol (b->name, strlen (b->name));
        s->type = TYPE_BUILTIN;
        s->value = b->func;
        b++;
    }

for (i = 0; i < 6; i++) {
    term_puts ("Loading ENV.LISP...\n\r");
    cbm_open (3, 8, 3, "ENV.LISP");
    // TODO: Error checking (smk).
    cbm_k_chkin (3);
    while (env = lisp_read ()) {
        //lisp_print (env);
        //term_puts ("\n\r");
    }
    cbm_k_close (3);
}

    term_puts ("\n\rBye!\n\r");
    while (1); // Gone with TUNIX.
    return 0;
}
