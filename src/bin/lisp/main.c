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

void FASTCALL
bi_1arg (lispptr x, char * msg)
{
    if (!CONSP(x)
        || !NOT(CONSP(CDR(x))))
        bierror (msg);
    arg1 = CAR(x);
}

void FASTCALL
bi_2args (lispptr x, char * msg)
{
    if (!CONSP(x)
        || NOT(CONSP(arg2c = CDR(x)))
        || !NOT(CDR(arg2c)))
        bierror (msg);
    arg1 = CAR(x);
    arg2 = CAR(arg2c);
}

lispptr FASTCALL
bi_eq (lispptr x)
{
    bi_2args (x, "(eq obj obj)");
    return arg1 == arg2 ? t : nil;
}

lispptr FASTCALL
bi_not (lispptr x)
{
    bi_1arg (x, "(not obj)");
    return NOT(arg1) ? t : nil;
}

lispptr FASTCALL
bi_atom (lispptr x)
{
    bi_1arg (x, "(atom obj)");
    return CONSP(arg1) ? nil : t;
}

lispptr FASTCALL
bi_symbolp (lispptr x)
{
    bi_1arg (x, "(symbol? obj)");
    return SYMBOLP(arg1) ? t : nil;
}

lispptr FASTCALL
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

lispptr FASTCALL
bi_symbol_value (lispptr x)
{
    if (!CONSP(x)
        || !NOT((CDR(x)))
        || !SYMBOLP(arg1 = CAR(x)))
        bierror ("(symbol-value symbol)");
    return SYMBOL_VALUE(x);
}

lispptr FASTCALL
bi_quote (lispptr x)
{
    bi_1arg (x, "(quote obj)");
    return arg1;
}

lispptr FASTCALL
bi_consp (lispptr x)
{
    bi_1arg (x, "(cons? obj)");
    return CONSP(arg1) ? t : nil;
}

lispptr FASTCALL
bi_cons (lispptr x)
{
    bi_2args (x, "(cons obj obj)");
    return lisp_make_cons(arg1, arg2);
}

void
cxr_args (lispptr x, char * msg)
{
    if (!CONSP(x)
        || !NOT((CDR(x)))
        || !LISTP(arg1 = CAR(x)))
        bierror (msg);
}

lispptr FASTCALL
bi_car (lispptr x)
{
    cxr_args (x, "(car lst)");
    return LIST_CAR(arg1);
}

lispptr FASTCALL
bi_cdr (lispptr x)
{
    cxr_args (x, "(cdr lst)");
    return LIST_CDR(arg1);
}

void
rplac_args (lispptr x, char * msg)
{
    if (!CONSP(x)
        || !NOT(arg2c = CDR(x))
        || !NOT(CDR(arg2c))
        || !CONSP(arg2 = CAR(arg2c)))
        bierror (msg);
}

lispptr FASTCALL
bi_rplaca (lispptr x)
{
    rplac_args (x, "(rplaca obj cons)");
    return RPLACA(arg1, arg2);
}

lispptr FASTCALL
bi_rplacd (lispptr x)
{
    rplac_args (x, "(rplacd obj cons)");
    return RPLACD(arg1, arg2);
}

lispptr FASTCALL
bi_numberp (lispptr x)
{
    bi_1arg (x, "(number? obj)");
    return NUMBERP(CAR(x)) ? t : nil;
}

void FASTCALL
bi_arith_arg (lispptr x, char * msg)
{
    if (!CONSP(x)
        || !NUMBERP(arg1 = CAR(x))
        || !NOT(CONSP(CDR(x))))
        bierror (msg);
}

void FASTCALL
bi_arith_args (lispptr x, char * msg)
{
    if (!CONSP(x)
        || !NUMBERP(arg1 = CAR(x))
        || !NOT(arg2c = CDR(x))
        || !NOT(CDR(arg2c))
        || !NUMBERP(arg2 = CAR(arg2c)))
        bierror (msg);
}

lispptr FASTCALL
bi_equal (lispptr x)
{
    bi_arith_args (x, "(== num num)");
    return BOOL(NUMBER_VALUE(arg1) == NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_lt (lispptr x)
{
    bi_arith_args (x, "(< num num)");
    return BOOL(NUMBER_VALUE(arg1) < NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_lte (lispptr x)
{
    bi_arith_args (x, "(<= num num)");
    return BOOL(NUMBER_VALUE(arg1) <= NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_gt (lispptr x)
{
    bi_arith_args (x, "(> num num)");
    return BOOL(NUMBER_VALUE(arg1) > NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_gte (lispptr x)
{
    bi_arith_args (x, "(>= num num)");
    return BOOL(NUMBER_VALUE(arg1) >= NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_add (lispptr x)
{
    bi_arith_args (x, "(+ num num)");
    return lisp_make_number (NUMBER_VALUE(arg1) + NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_sub (lispptr x)
{
    bi_arith_args (x, "(- num num)");
    return lisp_make_number (NUMBER_VALUE(arg1) + NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_mul (lispptr x)
{
    bi_arith_args (x, "(* num num)");
    return lisp_make_number (NUMBER_VALUE(arg1) + NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_div (lispptr x)
{
    bi_arith_args (x, "(/ num num)");
    return lisp_make_number (NUMBER_VALUE(arg1) + NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_mod (lispptr x)
{
    bi_arith_args (x, "(% num num)");
    return lisp_make_number (NUMBER_VALUE(arg1) + NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_inc (lispptr x)
{
    bi_arith_arg (x, "(++ num)");
    return lisp_make_number (NUMBER_VALUE(arg1) + 1);
}

lispptr FASTCALL
bi_dec (lispptr x)
{
    bi_arith_arg (x, "(-- num)");
    return lisp_make_number (NUMBER_VALUE(arg1) - 1);
}

lispptr FASTCALL
bi_eval (lispptr x)
{
    if (!CONSP(x)
        || !NOT((CDR(x))))
        bierror ("(eval obj)");
    return eval (CAR(x));
}

lispptr FASTCALL
bi_apply (lispptr x)
{
    if (!CONSP(x)
        || NOT(arg2c = CDR(x))
        || !NOT(CDR(arg2c)))
        bierror ("(apply fun . args)");
    return lisp_make_cons(CAR(x), arg2);
}

lispptr return_tag;
lispptr go_tag;

lispptr FASTCALL
bi_block (lispptr x)
{
    lispptr res;
    lispptr p;
    lispptr tag;

    if (!CONSP(x)
        || !ATOM(arg1 = CAR(x))
        || !NOT(arg2c = CDR(x))
        || !NOT(CDR(arg2c)))
        bierror ("(block name . exprs)");

    for (p = arg2; !NOT(p); p = LIST_CDR(p)) {
        res = eval (CAR(p));
        if (CONSP(res)) {
            // Handle RETURN.
            if (CAR(res) == return_tag) {
                if (arg1 == CAR(CDR(res)))
                    return CDR(CDR(res));
                return res;
            }
            // Handle GO.
            if (CAR(res) == go_tag) {
                // Search tag in body.
                tag = CAR(CDR(res));
                for (p = arg2; !NOT(p); p = LIST_CDR(p)) {
                    if (CAR(p) == tag) {
                        p = CDR(p);
                        break;
                    }
                }
                return res;
            }
        }
    }
    return res;
}

lispptr FASTCALL
bi_return (lispptr x)
{
    if (!CONSP(x))
        bierror ("(return obj [name])");
    // TODO: Re-use list.
    return lisp_make_cons (return_tag, lisp_make_cons (CAR(x), LIST_CAR(LIST_CDR(x))));
}

lispptr FASTCALL
bi_go (lispptr x)
{
    bi_1arg (x, "(go tag)");
    // TODO: Re-use cons.
    return lisp_make_cons (go_tag, arg1);
}

lispptr FASTCALL
bi_if (lispptr x)
{
    if (!CONSP(x)
        || NOT(CONSP(arg2c = CDR(x)))
        || !NOT(CDR(arg2c)))
        bierror ("(? cond obj [cond obj/default])");
    while (!NOT(x)) {
        arg1 = CAR(x);
        if (NOT(arg2c = CDR(x)))
            return eval (arg1);
        if (eval (arg1))
            return eval (CAR(arg2c));
        x = CDR(arg2c);
    }
    // NOTREACHED, I hope...
    bierror ("?: default missing.");
}

lispptr FASTCALL
bi_and (lispptr x)
{
    for (;!NOT(x); x = LIST_CDR(x))
        if (NOT(CAR(x)))
            return nil;
    return t;
}

lispptr FASTCALL
bi_or (lispptr x)
{
    for (;!NOT(x); x = LIST_CDR(x))
        if (!NOT(CAR(x)))
            return t;
    return nil;
}

lispptr FASTCALL
bi_read (lispptr x)
{
    if (!NOT(x))
        bierror ("(read)");
    return lisp_read ();
}

lispptr FASTCALL
bi_print (lispptr x)
{
    return lisp_print (LIST_CAR(x));
}

struct builtin builtins[] = {
    { "apply",      bi_apply },
    { "eval",       bi_eval },
    { "?",          bi_if },
    { "&",          bi_and },
    { "|",          bi_or },
    { "block",      bi_block },
    { "return",     bi_return },
    { "go",         bi_go },

    { "not",        bi_not },
    { "eq",         bi_eq },
    { "atom",       bi_atom },

    { "symbol?",      bi_symbolp },
    { "set",          bi_set },
    { "symbol-value", bi_symbol_value },

    { "cons?",      bi_consp },
    { "cons",       bi_cons },
    { "car",        bi_car },
    { "cdr",        bi_cdr },
    { "rplaca",     bi_rplaca },
    { "rplacd",     bi_rplacd },

    { "quote",      bi_quote },
    { "backquote",  NULL },
    { "quasiquote", NULL },
    { "quasiquote-splice", NULL },

    { "number?",    bi_numberp },

    { "==",         bi_equal },
    { ">",          bi_gt },
    { "<",          bi_lt },
    { ">=",         bi_gte },
    { "<=",         bi_lte },

    { "+",          bi_add },
    { "-",          bi_sub },
    { "*",          bi_mul },
    { "/",          bi_div },
    { "%",          bi_mod },
    { "++",         bi_inc },
    { "--",         bi_dec },

    { "bit-and",    NULL },
    { "bit-or",     NULL },
    { "bit-xor",    NULL },
    { "bit-neg",    NULL },
    { ">>",         NULL },
    { "<<",         NULL },

    { "peek",       NULL },
    { "poke",       NULL },
    { "sys",        NULL },

    { "read",       bi_read },
    { "print",      bi_print },

    { NULL, NULL }
};

int
main (int argc, char * argv[])
{
    lispptr x;
    (void) argc, (void) argv;

    term_init ();
    lisp_init ();
    return_tag = lisp_make_symbol ("%R", 2);
    go_tag = lisp_make_symbol ("%G", 2);
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
