#ifndef __CBM__
#define __CBM__
#endif

#include <ingle/cc65-charmap.h>

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include <cbm.h>

#include <term/libterm.h>
#include <lisp/liblisp.h>

lispptr
bi_print (lispptr x)
{
    return lisp_print (LIST_CAR(x));
}

struct builtin builtins[] = {
    { "?", NULL },
    { "&", NULL },
    { "|", NULL },
    { "block", NULL },
    { "eq", NULL },
    { "atom", NULL },
    { "not", NULL },
    { "cons", NULL },
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
    { "print", bi_print },
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

int
main (int argc, char * argv[])
{
    lispptr x;
    (void) argc, (void) argv;

    term_init ();
    lisp_init ();
    add_builtins (builtins);

    term_puts ("Loading ENV.LISP...\n\r");
    cbm_open (3, 8, 3, "ENV.LISP");
    // TODO: Error check.
    cbm_k_chkin (3);
    while (x = lisp_read ()) {
        lisp_print (x);
        term_puts ("\n\r");
        x = eval (x);
        term_puts ("\n\r");
        lisp_print (x);
    }
    cbm_k_close (3);

    term_puts ("\n\rBye!\n\r");
    while (1); // Gone with terminal compiled in.
    return 0;
}
