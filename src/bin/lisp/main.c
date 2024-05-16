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

lispptr * stack;

#define PUSH(x)     (*--stack = (lispptr) x)
#define POP(x)      (x = *++stack)
#define PUSHSIZE(x) PUSH(x)
#define POPSIZE(x)  (x = (unsigned) *++stack)

lispptr
eval (lispptr x)
{
    return x;
}

lispptr
eval_list (lispptr x)
{
    return x;
}

lispptr
eval_body (lispptr x)
{
    return x;
}

// Function call
//
// 'fun' is a cons with the argument definition in CAR and
// body expressions in CDR.
lispptr
apply (lispptr fun, lispptr args, bool do_eval)
{
    lispptr argdef = FUNARGS(fun);
    lispptr ad;
    lispptr av;
    lispptr name;
    lispptr rest;
    lispptr value;
    unsigned stsize;

    // Push argument symbol values onto the stack.
    for (ad = argdef, av = args, stsize = 0;
         ad != nil && av != nil;
         ad = CDR(ad), av = CDR(av)) {
        stsize++;

        // Rest of argument list. (consing)
        if (ATOM(ad)) {
            PUSH(ad);
            PUSH(SYMBOL_VALUE(ad));
            rest = do_eval ? eval_list (av) : av;
            SET_SYMBOL_VALUE(ad, rest);
            break;
        }

        name = CAR(ad);
        PUSH(name);
        PUSH(SYMBOL_VALUE(name));
        value = do_eval ? eval (CAR(av)) : CAR(av);
        SET_SYMBOL_VALUE(name, value);
    }

    PUSHSIZE(stsize);
    value = eval_body (FUNBODY(fun));
    POPSIZE(stsize);

    // Pop argument symbol values from the stack.
    while (stsize--) {
        POP(name);
        POP(SYMBOL_VALUE(name));
    }

    return value;
}

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
        s->type = TYPE_BUILTIN;
        s->value = b->func;
        b++;
    }

    term_puts ("Loading ENV.LISP...\n\r");
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
