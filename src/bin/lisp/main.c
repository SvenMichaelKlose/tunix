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

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
lispptr arg1;
lispptr arg2c;
lispptr arg2;
#ifdef __CC65__
#pragma zpsym ("arg1")
#pragma zpsym ("arg2c")
#pragma zpsym ("arg2")
#pragma bss-name (pop)
#endif

void
fnord (void)
{
}

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
        || !NOT(CDR(x)))
        bierror (msg);
    arg1 = eval (CAR(x));
}

void FASTCALL
bi_2args (lispptr x, char * msg)
{
    if (!CONSP(x)
        || NOT(CONSP(arg2c = CDR(x)))
        || !NOT(CDR(arg2c)))
        bierror (msg);
    arg1 = eval (CAR(x));
    arg2 = eval (CAR(arg2c));
}

lispptr FASTCALL
bi_eq (lispptr x)
{
    bi_2args (x, "(eq x x)");
    return arg1 == arg2 ? t : nil;
}

lispptr FASTCALL
bi_not (lispptr x)
{
    bi_1arg (x, "(not x)");
    return NOT(arg1) ? t : nil;
}

lispptr FASTCALL
bi_atom (lispptr x)
{
    bi_1arg (x, "(atom x)");
    return CONSP(arg1) ? nil : t;
}

lispptr FASTCALL
bi_symbolp (lispptr x)
{
    bi_1arg (x, "(symbol? x)");
    return SYMBOLP(arg1) ? t : nil;
}

lispptr FASTCALL
bi_setq (lispptr x)
{
    if (!CONSP(x)
        || !CONSP(arg2c = LIST_CDR(x))
        || !SYMBOLP(arg1 = CAR(x))
        || !NOT(CDR(arg2c)))
        bierror ("(setq sym x)");
    SET_SYMBOL_VALUE(arg1, arg2 = eval (CAR(arg2c)));
    return arg2;
}

lispptr FASTCALL
bi_symbol_value (lispptr x)
{
    if (!CONSP(x)
        || !NOT((CDR(x)))
        || !SYMBOLP(arg1 = eval (CAR(x))))
        bierror ("(symbol-value symbol)");
    return SYMBOL_VALUE(arg1);
}

lispptr FASTCALL
bi_quote (lispptr x)
{
    if (!CONSP(x)
        || !NOT(CDR(x)))
        bierror ("(quote x)");
    return CAR(x);
}

lispptr FASTCALL
bi_consp (lispptr x)
{
    bi_1arg (x, "(cons? x)");
    return CONSP(arg1) ? t : nil;
}

lispptr FASTCALL
bi_cons (lispptr x)
{
    bi_2args (x, "(cons x x)");
    return lisp_make_cons(arg1, arg2);
}

void
cxr_args (lispptr x, char * msg)
{
    if (!CONSP(x)
        || !NOT((CDR(x)))
        || !LISTP(arg1 = eval (CAR(x))))
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
    if (!CONSP(x))
        bierror (msg);
    arg1 = eval (CAR(x));
    if (!NOT(arg2c = eval (CDR(x)))
        || !NOT(CDR(arg2c))
        || !CONSP(arg2 = CAR(arg2c)))
        bierror (msg);
}

lispptr FASTCALL
bi_rplaca (lispptr x)
{
    rplac_args (x, "(rplaca x cons)");
    return RPLACA(arg1, arg2);
}

lispptr FASTCALL
bi_rplacd (lispptr x)
{
    rplac_args (x, "(rplacd x cons)");
    return RPLACD(arg1, arg2);
}

lispptr FASTCALL
bi_numberp (lispptr x)
{
    bi_1arg (x, "(number? x)");
    return NUMBERP(CAR(x)) ? t : nil;
}

void FASTCALL
bi_arith_arg (lispptr x, char * msg)
{
    if (!CONSP(x)
        || !NUMBERP(arg1 = eval (CAR(x)))
        || !NOT(CDR(x)))
        bierror (msg);
}

void FASTCALL
bi_arith_args (lispptr x, char * msg)
{
    if (!CONSP(x)
        || !NUMBERP(arg1 = CAR(x))
        || NOT(arg2c = CDR(x))
        || !NUMBERP(arg2 = CAR(arg2c))
        || !NOT(CDR(arg2c)))
        bierror (msg);
}

void FASTCALL
bi_arith_many (lispptr x, char * msg)
{
    if (!CONSP(x)
        || !NUMBERP(arg1 = eval (CAR(x)))
        || NOT(arg2c = CDR(x)))
        bierror (msg);
}

lispptr FASTCALL
bi_equal (lispptr x)
{
    bi_arith_args (x, "(== n n)");
    return BOOL(NUMBER_VALUE(arg1) == NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_lt (lispptr x)
{
    bi_arith_args (x, "(< n n)");
    return BOOL(NUMBER_VALUE(arg1) < NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_lte (lispptr x)
{
    bi_arith_args (x, "(<= n n)");
    return BOOL(NUMBER_VALUE(arg1) <= NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_gt (lispptr x)
{
    bi_arith_args (x, "(> n n)");
    return BOOL(NUMBER_VALUE(arg1) > NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_gte (lispptr x)
{
    bi_arith_args (x, "(>= n n)");
    return BOOL(NUMBER_VALUE(arg1) >= NUMBER_VALUE(arg2));
}

#define DOLIST(x, init) \
    for (x = init; !NOT(x); x = LIST_CDR(x))

#define DEFARITH(fun_name, op, err) \
lispptr FASTCALL \
fun_name (lispptr x) \
{ \
    static char * msg = err; \
    int v; \
    lispptr n; \
    bi_arith_many (x, err); \
    v = NUMBER_VALUE(arg1); \
    DOLIST(x, arg2c) { \
        if (!NUMBERP(n = eval (CAR(x)))) \
            bierror (msg); \
        v op NUMBER_VALUE(n); \
    } \
    return lisp_make_number (v); \
}

DEFARITH(bi_add, +=, "(+ n n...)");
DEFARITH(bi_sub, -=, "(- n n...)");
DEFARITH(bi_mul, *=, "(* n n...)");
DEFARITH(bi_div, /=, "(/ n n...)");
DEFARITH(bi_mod, %=, "(% n n...)");

lispptr FASTCALL
bi_inc (lispptr x)
{
    bi_arith_arg (x, "(++ n)");
    return lisp_make_number (NUMBER_VALUE(arg1) + 1);
}

lispptr FASTCALL
bi_dec (lispptr x)
{
    bi_arith_arg (x, "(-- n)");
    return lisp_make_number (NUMBER_VALUE(arg1) - 1);
}

lispptr FASTCALL
bi_eval (lispptr x)
{
    bi_1arg (x, "(eval x)");
    return eval (arg1);
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
        || !SYMBOLP(arg1 = CAR(x))
        || !NOT(arg2c = CDR(x))
        || !NOT(CDR(arg2c)))
        bierror ("(block name . exprs)");

    DOLIST(p, arg2) {
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
        bierror ("(return x [name])");
    // TODO: Re-use list.
    return lisp_make_cons (return_tag, lisp_make_cons (eval (CAR(x)), eval (LIST_CAR(LIST_CDR(x)))));
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
        || NOT(CDR(arg2c)))
        bierror ("(? cond x [cond x/default])");
    while (!NOT(x)) {
        arg1 = CAR(x);
        if (NOT(arg2c = CDR(x)))
            return eval (arg1);
        if (!NOT(eval (arg1)))
            return eval (CAR(arg2c));
        x = CDR(arg2c);
    }
    // NOTREACHED, I hope...
    bierror ("?: default missing.");
}

lispptr FASTCALL
bi_and (lispptr x)
{
    DOLIST(x, x)
        if (NOT(eval (CAR(x))))
            return nil;
    return t;
}

lispptr FASTCALL
bi_or (lispptr x)
{
    DOLIST(x, x)
        if (!NOT(eval (CAR(x))))
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
    bi_1arg (x, "(print x)");
    return lisp_print (arg1);
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
    { "setq",         bi_setq },
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
    lisp_print (universe);
    gc ();
    lisp_print (universe);

    term_puts ("\n\rLoading ENV.LISP...\n\r");
    cbm_open (3, 8, 3, "ENV.LISP");
    // TODO: Error check.
    cbm_k_chkin (3);
    while (x = lisp_read ()) {
        term_puts ("; ");
        lisp_print (x);
        term_puts ("\n\r");
        x = eval (x);
        lisp_print (x);
        term_puts ("\n\r");
        term_puts ("GC");
        gc ();
        term_puts ("\n\r");
    }
    cbm_k_close (3);

    term_puts ("\n\rBye!\n\r");
    while (1); // Gone with terminal compiled in.
    return 0;
}
