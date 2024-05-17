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
#include <lisp/io.h>

lispptr arg1;
lispptr arg2c;
lispptr arg2;

void
bierror (char * msg)
{
    outs ("ERROR: ");
    outs (msg);
    outs ("\n\r");
    while (1);
}

lispptr
bi_eq (lispptr x)
{
    if (!CONSP(x)
        || !NOT(arg2c = CDR(x))
        || !NOT(CDR(arg2c)))
        bierror ("(cons obj obj)");
    return CAR(x) == arg2 ? t : nil;
}

lispptr
bi_not (lispptr x)
{
    if (!CONSP(x)
        || !NOT((CDR(x))))
        bierror ("(not obj)");
    return NOT(CAR(x)) ? t : nil;
}

lispptr
bi_atom (lispptr x)
{
    if (!CONSP(x)
        || !NOT((CDR(x))))
        bierror ("(atom obj)");
    return CONSP(CAR(x)) ? nil : t;
}

lispptr
bi_symbolp (lispptr x)
{
    if (!CONSP(x)
        || !NOT((CDR(x))))
        bierror ("(symbol? obj)");
    return SYMBOLP(CAR(x)) ? t : nil;
}

lispptr
bi_set (lispptr x)
{
    if (!CONSP(arg1 = LIST_CAR(x))
        || !CONSP(arg2c = LIST_CDR(x))
        || !SYMBOLP(arg1)
        || !NOT(CDR(arg2c)))
        bierror ("(set sym obj)");
    SET_SYMBOL_VALUE(arg1, arg2 = CAR(arg2c));
    return arg2;
}

lispptr
bi_quote (lispptr x)
{
    if (!CONSP(x)
        || !NOT((CDR(x))))
        bierror ("(quote obj)");
    return CAR(x);
}

lispptr
bi_consp (lispptr x)
{
    if (!CONSP(x)
        || !NOT((CDR(x))))
        bierror ("(atom obj)");
    return CONSP(CAR(x)) ? t : nil;
}

lispptr
bi_cons (lispptr x)
{
    if (!CONSP(x)
        || !NOT(arg2c = CDR(x))
        || !NOT(CDR(arg2c)))
        bierror ("(cons obj obj)");
    return lisp_make_cons(CAR(x), arg2);
}

lispptr
bi_car (lispptr x)
{
    if (!CONSP(x)
        || !NOT((CDR(x)))
        || !LISTP(arg1 = CAR(x)))
        bierror ("(car lst)");
    return LIST_CAR(arg1);
}

lispptr
bi_cdr (lispptr x)
{
    if (!CONSP(x)
        || !NOT((CDR(x)))
        || !LISTP(arg1 = CAR(x)))
        bierror ("(cdr lst)");
    return LIST_CDR(arg1);
}

lispptr
bi_rplaca (lispptr x)
{
    if (!CONSP(x)
        || !NOT(arg2c = CDR(x))
        || !NOT(CDR(arg2c))
        || !CONSP(arg2 = CAR(arg2c)))
        bierror ("(rplaca obj cons)");
    return RPLACA(CAR(x), arg2);
}

lispptr
bi_rplacd (lispptr x)
{
    if (!CONSP(x)
        || !NOT(arg2c = CDR(x))
        || !NOT(CDR(arg2c))
        || !CONSP(arg2 = CAR(arg2c)))
        bierror ("(rplacd obj cons)");
    return RPLACD(CAR(x), arg2);
}

lispptr
bi_numberp (lispptr x)
{
    if (!CONSP(x)
        || !NOT((CDR(x))))
        bierror ("(number? obj)");
    return NUMBERP(CAR(x)) ? t : nil;
}

void
bi_arith_arg (lispptr x, char * msg)
{
    if (!CONSP(x)
        || !NUMBERP(arg1 = CAR(x))
        || !NOT(CONSP(CDR(x))))
        bierror (msg);
}

void
bi_arith_args (lispptr x, char * msg)
{
    if (!CONSP(x)
        || !NUMBERP(arg1 = CAR(x))
        || !NOT(arg2c = CDR(x))
        || !NOT(CDR(arg2c))
        || !NUMBERP(arg2 = CAR(arg2c)))
        bierror (msg);
}

lispptr
bi_equal (lispptr x)
{
    bi_arith_args (x, "(== num num)");
    return BOOL(NUMBER_VALUE(arg1) == NUMBER_VALUE(arg2));
}

lispptr
bi_lt (lispptr x)
{
    bi_arith_args (x, "(< num num)");
    return BOOL(NUMBER_VALUE(arg1) < NUMBER_VALUE(arg2));
}

lispptr
bi_lte (lispptr x)
{
    bi_arith_args (x, "(<= num num)");
    return BOOL(NUMBER_VALUE(arg1) <= NUMBER_VALUE(arg2));
}

lispptr
bi_gt (lispptr x)
{
    bi_arith_args (x, "(> num num)");
    return BOOL(NUMBER_VALUE(arg1) > NUMBER_VALUE(arg2));
}

lispptr
bi_gte (lispptr x)
{
    bi_arith_args (x, "(>= num num)");
    return BOOL(NUMBER_VALUE(arg1) >= NUMBER_VALUE(arg2));
}

lispptr
bi_add (lispptr x)
{
    bi_arith_args (x, "(+ num num)");
    return lisp_make_number (NUMBER_VALUE(arg1) + NUMBER_VALUE(arg2));
}

lispptr
bi_sub (lispptr x)
{
    bi_arith_args (x, "(- num num)");
    return lisp_make_number (NUMBER_VALUE(arg1) + NUMBER_VALUE(arg2));
}

lispptr
bi_mul (lispptr x)
{
    bi_arith_args (x, "(* num num)");
    return lisp_make_number (NUMBER_VALUE(arg1) + NUMBER_VALUE(arg2));
}

lispptr
bi_div (lispptr x)
{
    bi_arith_args (x, "(/ num num)");
    return lisp_make_number (NUMBER_VALUE(arg1) + NUMBER_VALUE(arg2));
}

lispptr
bi_mod (lispptr x)
{
    bi_arith_args (x, "(% num num)");
    return lisp_make_number (NUMBER_VALUE(arg1) + NUMBER_VALUE(arg2));
}

lispptr
bi_inc (lispptr x)
{
    bi_arith_arg (x, "(++ num)");
    return lisp_make_number (NUMBER_VALUE(arg1) + 1);
}

lispptr
bi_dec (lispptr x)
{
    bi_arith_arg (x, "(-- num)");
    return lisp_make_number (NUMBER_VALUE(arg1) - 1);
}

lispptr
bi_eval (lispptr x)
{
    if (!CONSP(x)
        || !NOT((CDR(x))))
        bierror ("(eval obj)");
    return eval (CAR(x));
}

lispptr
bi_apply (lispptr x)
{
    if (!CONSP(x)
        || !NOT(arg2c = CDR(x))
        || !NOT(CDR(arg2c)))
        bierror ("(apply fun . args)");
    return lisp_make_cons(CAR(x), arg2);
}

lispptr
bi_read (lispptr x)
{
    if (!NOT(x))
        bierror ("(read)");
    return lisp_read ();
}

lispptr
bi_print (lispptr x)
{
    return lisp_print (LIST_CAR(x));
}

struct builtin builtins[] = {
    { "apply", bi_apply },
    { "eval", bi_eval },
    { "?", NULL },
    { "&", NULL },
    { "|", NULL },
    { "block", NULL },
    { "return", NULL },

    { "not", bi_not },
    { "eq", bi_eq },
    { "atom", bi_atom },

    { "symbol?", bi_symbolp },
    { "set", bi_set },

    { "cons?", bi_consp },
    { "cons", bi_cons },
    { "car", bi_car },
    { "cdr", bi_cdr },
    { "rplaca", bi_rplaca },
    { "rplacd", bi_rplacd },

    { "quote", bi_quote },
    { "backquote", NULL },
    { "quasiquote", NULL },
    { "quasiquote-splice", NULL },

    { "number?", bi_numberp },

    { "==", bi_equal },
    { ">", bi_gt },
    { "<", bi_lt },
    { ">=", bi_gte },
    { "<=", bi_lte },

    { "+", bi_add },
    { "-", bi_sub },
    { "*", bi_mul },
    { "/", bi_div },
    { "%", bi_mod },
    { "++", bi_inc },
    { "--", bi_dec },

    { "bit-and", NULL },
    { "bit-or", NULL },
    { "bit-xor", NULL },
    { "bit-neg", NULL },
    { ">>", NULL },
    { "<<", NULL },

    { "peek", NULL },
    { "poke", NULL },
    { "sys", NULL },

    { "read", bi_read },
    { "print", bi_print },

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
