#ifndef __CBM__
#define __CBM__
#endif

#include <ingle/cc65-charmap.h>
#include <cbm.h>

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

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

extern void error (char * msg);

void
error (char * msg)
{
    errouts ("ERROR: ");
    outs (msg);
    outs ("\n\r");
    while (1);
}

void
bierror (char * msg)
{
    error (msg);
}

int
length (lispptr * x)
{
    int len = 0;
    for(; x; x = CDR(x))
        len++;
    return len;
}

void FASTCALL
ensure_one_arg (lispptr x, char * msg)
{
    if (!CONSP(x)
        || CDR(x))
        bierror (msg);
    arg1 = eval (CAR(x));
}

void FASTCALL
ensure_two_args (lispptr x, char * msg)
{
    if (!CONSP(x)
        || !CONSP(arg2c = CDR(x))
        || CDR(arg2c))
        bierror (msg);
    arg1 = eval (CAR(x));
    PUSH(arg1);
    arg2 = eval (CAR(arg2c));
    POP(arg1);
}

void FASTCALL
ensure_one_number (lispptr x, char * msg)
{
    if (!CONSP(x)
        || !NUMBERP(arg1 = eval (CAR(x)))
        || CDR(x))
        bierror (msg);
}

void FASTCALL
ensure_two_numbers (lispptr x, char * msg)
{
    if (!CONSP(x)
        || !NUMBERP(arg1 = CAR(x))
        || !(arg2c = CDR(x))
        || !NUMBERP(arg2 = CAR(arg2c))
        || CDR(arg2c))
        bierror (msg);
}

void FASTCALL
ensure_numbers (lispptr x, char * msg)
{
    if (!CONSP(x)
        || !NUMBERP(arg1 = eval (CAR(x)))
        || !(arg2c = CDR(x)))
        bierror (msg);
}

void FASTCALL
cons_getter_args (lispptr x, char * msg)
{
    if (!CONSP(x)
        || CDR(x)
        || !LISTP(arg1 = eval (CAR(x))))
        bierror (msg);
}

void FASTCALL
cons_setter_args (lispptr x, char * msg)
{
    if (!CONSP(x))
        bierror (msg);
    arg1 = eval (CAR(x));
    PUSH(arg1);
    if ((arg2c = eval (CDR(x)))
        || CDR(arg2c)
        || !CONSP(arg2 = CAR(arg2c)))
        bierror (msg);
    POP(arg1);
}

lispptr FASTCALL
bi_eq (lispptr x)
{
    ensure_two_args (x, "(eq x x)");
    return arg1 == arg2 ? t : nil;
}

lispptr FASTCALL
bi_not (lispptr x)
{
    ensure_one_arg (x, "(not x)");
    return !arg1 ? t : nil;
}

lispptr FASTCALL
bi_atom (lispptr x)
{
    ensure_one_arg (x, "(atom x)");
    return CONSP(arg1) ? nil : t;
}

lispptr FASTCALL
bi_symbolp (lispptr x)
{
    ensure_one_arg (x, "(symbol? x)");
    return SYMBOLP(arg1) ? t : nil;
}

lispptr FASTCALL
bi_setq (lispptr x)
{
    if (!CONSP(x)
        || !CONSP(arg2c = LIST_CDR(x))
        || !SYMBOLP(arg1 = CAR(x))
        || CDR(arg2c))
        bierror ("(setq sym x)");
    SET_SYMBOL_VALUE(arg1, arg2 = eval (CAR(arg2c)));
    return arg2;
}

lispptr FASTCALL
bi_symbol_value (lispptr x)
{
    if (!CONSP(x)
        || (CDR(x))
        || !SYMBOLP(arg1 = eval (CAR(x))))
        bierror ("(symbol-value symbol)");
    return SYMBOL_VALUE(arg1);
}

lispptr FASTCALL
bi_string (lispptr x)
{
    int len;
    lispptr s;
    char * p;
    ensure_one_arg (x, "(string nlst)");
    len = length (arg1);
    s = lisp_alloc_symbol (buffer, len);
    p = SYMBOL_NAME(s);
    for (; arg1; arg1 = CDR(arg1)) {
        if (!NUMBERP(CAR(arg1)))
            bierror ("(string nlst)");
        *p++ = NUMBER_VALUE(CAR(arg1));
    }
    return s;
}

lispptr FASTCALL
bi_quote (lispptr x)
{
    if (!CONSP(x)
        || CDR(x))
        bierror ("(quote x)");
    return CAR(x);
}

lispptr FASTCALL
bi_consp (lispptr x)
{
    ensure_one_arg (x, "(cons? x)");
    return CONSP(arg1) ? t : nil;
}

lispptr FASTCALL
bi_cons (lispptr x)
{
    ensure_two_args (x, "(cons x x)");
    return lisp_make_cons(arg1, arg2);
}

lispptr FASTCALL
bi_car (lispptr x)
{
    cons_getter_args (x, "(car lst)");
    return LIST_CAR(arg1);
}

lispptr FASTCALL
bi_cdr (lispptr x)
{
    cons_getter_args (x, "(cdr lst)");
    return LIST_CDR(arg1);
}

lispptr FASTCALL
bi_rplaca (lispptr x)
{
    cons_setter_args (x, "(rplaca x c)");
    return RPLACA(arg1, arg2);
}

lispptr FASTCALL
bi_rplacd (lispptr x)
{
    cons_setter_args (x, "(rplacd x c)");
    return RPLACD(arg1, arg2);
}

lispptr FASTCALL
bi_numberp (lispptr x)
{
    ensure_one_arg (x, "(number? x)");
    return NUMBERP(CAR(x)) ? t : nil;
}

lispptr FASTCALL
bi_equal (lispptr x)
{
    ensure_two_numbers (x, "(== n n)");
    return BOOL(NUMBER_VALUE(arg1) == NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_lt (lispptr x)
{
    ensure_two_numbers (x, "(< n n)");
    return BOOL(NUMBER_VALUE(arg1) < NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_lte (lispptr x)
{
    ensure_two_numbers (x, "(<= n n)");
    return BOOL(NUMBER_VALUE(arg1) <= NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_gt (lispptr x)
{
    ensure_two_numbers (x, "(> n n)");
    return BOOL(NUMBER_VALUE(arg1) > NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_gte (lispptr x)
{
    ensure_two_numbers (x, "(>= n n)");
    return BOOL(NUMBER_VALUE(arg1) >= NUMBER_VALUE(arg2));
}

#define DOLIST(x, init) \
    for (x = init; x; x = LIST_CDR(x))

#define DEFARITH(fun_name, op, err) \
lispptr FASTCALL \
fun_name (lispptr x) \
{ \
    static char * msg = err; \
    int v; \
    lispptr n; \
    ensure_numbers (x, err); \
    v = NUMBER_VALUE(arg1); \
    DOLIST(x, arg2c) { \
        PUSH(x); \
        if (!NUMBERP(n = eval (CAR(x)))) \
            bierror (msg); \
        POP(x); \
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
    ensure_one_number (x, "(++ n)");
    return lisp_make_number (NUMBER_VALUE(arg1) + 1);
}

lispptr FASTCALL
bi_dec (lispptr x)
{
    ensure_one_number (x, "(-- n)");
    return lisp_make_number (NUMBER_VALUE(arg1) - 1);
}

lispptr FASTCALL
bi_bit_and (lispptr x)
{
    ensure_two_numbers (x, "(bit-and n n)");
    return lisp_make_number (NUMBER_VALUE(arg1) & NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_bit_or (lispptr x)
{
    ensure_two_numbers (x, "(bit-or n n)");
    return lisp_make_number (NUMBER_VALUE(arg1) | NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_bit_xor (lispptr x)
{
    ensure_two_numbers (x, "(bit-xor n n)");
    return lisp_make_number (NUMBER_VALUE(arg1) ^ NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_bit_neg (lispptr x)
{
    ensure_one_number (x, "(bit-neg n)");
    return lisp_make_number (~NUMBER_VALUE(arg1));
}

lispptr FASTCALL
bi_shift_left (lispptr x)
{
    ensure_two_numbers (x, "(<< n nbits)");
    return lisp_make_number (NUMBER_VALUE(arg1) << NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_shift_right (lispptr x)
{
    ensure_two_numbers (x, "(>> n nbits)");
    return lisp_make_number (NUMBER_VALUE(arg1) >> NUMBER_VALUE(arg2));
}

lispptr FASTCALL
bi_peek (lispptr x)
{
    ensure_one_number (x, "(peek addr)");
    return lisp_make_number (*(char *) NUMBER_VALUE(arg1));
}

lispptr FASTCALL
bi_poke (lispptr x)
{
    ensure_two_numbers (x, "(poke addr b)");
    *(char *) NUMBER_VALUE(arg1) = NUMBER_VALUE(arg2);
    return arg2;
}

lispptr FASTCALL
bi_sys (lispptr x)
{
    ensure_one_number (x, "(sys addr)");
    ((void (*) (void)) NUMBER_VALUE(arg1)) ();
    return nil;
}

lispptr FASTCALL
bi_eval (lispptr x)
{
    ensure_one_arg (x, "(eval x)");
    return eval (arg1);
}

lispptr FASTCALL
bi_apply (lispptr x)
{
    if (!CONSP(x)
        || !(arg2c = CDR(x))
        || CDR(arg2c))
        bierror ("(apply fun . args)");
    return apply (CAR(x), arg2c, true);
}

lispptr return_tag;
lispptr go_tag;

lispptr FASTCALL
bi_block (lispptr x)
{
    lispptr res = nil;
    lispptr p;
    lispptr tag;
    bool    tag_found;

    if (!CONSP(x) || !SYMBOLP(arg1 = CAR(x)))
        bierror ("(block name . exprs)");
    arg2c = CDR(x);

    DOLIST(p, arg2c) {
        PUSH(arg2c);
        PUSH(p);
        res = eval (CAR(p));
        POP(p);
        POP(arg2c);
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
                res = nil;
                tag_found = false;
                for (p = arg2c; p; p = LIST_CDR(p)) {
                    if (CAR(p) == tag) {
                        tag_found = true;
                        break;
                    }
                }
                if (!tag_found)
                    error ("Tag not found.");
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
    arg1 = eval (CAR(x));
    PUSH(arg1);
    arg2 = eval (LIST_CAR(LIST_CDR(x)));
    POP(arg1);
    arg1 = lisp_make_cons (arg1, arg2);
    return lisp_make_cons (return_tag, arg1);
}

lispptr FASTCALL
bi_go (lispptr x)
{
    ensure_one_arg (x, "(go tag)");
    // TODO: Re-use cons.
    return lisp_make_cons (go_tag, arg1);
}

lispptr tmp;

lispptr FASTCALL
bi_if (lispptr x)
{
    if (!CONSP(x) || !CONSP(arg2c = CDR(x)))
        bierror ("(? cond x [cond x/default])");
    while (x) {
        arg1 = CAR(x);
        if (!(arg2c = CDR(x)))
            return eval (arg1);
        PUSH(arg2c);
        tmp = eval (arg1);
        POP(arg2c);
        if (tmp)
            return eval (CAR(arg2c));
        x = CDR(arg2c);
    }
    /* NOTREACHED */
    bierror ("?: default missing.");
}

lispptr FASTCALL
bi_and (lispptr x)
{
    DOLIST(x, x) {
        PUSH(x);
        if (!eval (CAR(x))) {
            POP(x);
            return nil;
        }
        POP(x);
    }
    return t;
}

lispptr FASTCALL
bi_or (lispptr x)
{
    DOLIST(x, x) {
        PUSH(x);
        if (eval (CAR(x))) {
            POP(x);
            return t;
        }
        POP(x);
    }
    return nil;
}

lispptr FASTCALL
bi_read (lispptr x)
{
    if (x)
        bierror ("(read)");
    return lisp_read ();
}

lispptr FASTCALL
bi_print (lispptr x)
{
    ensure_one_arg (x, "(print x)");
    return lisp_print (arg1);
}

lispptr FASTCALL
bi_princ (lispptr x)
{
    ensure_one_arg (x, "(princ x)");
    if (NUMBERP(arg1))
        out (NUMBER_VALUE(arg1));
    else if (SYMBOLP(arg1))
        outsn (SYMBOL_NAME(arg1), SYMBOL_LENGTH(arg1));
    else
        lisp_print (arg1);
    return arg1;
}

lispptr FASTCALL
bi_fn (lispptr x)
{
    if (!CONSP(x)
        || !SYMBOLP(arg1 = CAR(x))
        || !CONSP(arg2c = CDR(x)))
        bierror ("(fn name obj)");
    EXPAND_UNIVERSE(arg1);
    SET_SYMBOL_VALUE(arg1, arg2c);
    return nil;
}

lispptr FASTCALL
bi_var (lispptr x)
{
    if (!CONSP(x)
        || !SYMBOLP(arg1 = CAR(x))
        || !CONSP(arg2c = CDR(x))
        || CDR(arg2c))
        bierror ("(var name obj)");
    EXPAND_UNIVERSE(arg1);
    PUSH(arg1);
    SET_SYMBOL_VALUE(arg1, eval (CAR(arg2c)));
    POP(arg1);
    return nil;
}

lispptr FASTCALL
bi_gc (lispptr x)
{
    (void) x;
    gc ();
    return lisp_make_number (heap_end - heap_free);
}

lispptr FASTCALL
bi_exit (lispptr x)
{
    ensure_one_number (x, "(exit n)");
    exit (NUMBER_VALUE(arg1));
    /* NOTREACHED */
    return nil;
}

struct builtin builtins[] = {
    { "quote",      bi_quote },

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
    { "cons?",      bi_consp },
    { "number?",    bi_numberp },
    { "symbol?",    bi_symbolp },

    { "setq",         bi_setq },
    { "symbol-value", bi_symbol_value },
    { "string",       bi_string },

    { "cons",       bi_cons },
    { "car",        bi_car },
    { "cdr",        bi_cdr },
    { "rplaca",     bi_rplaca },
    { "rplacd",     bi_rplacd },

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

    { "bit-and",    bi_bit_and },
    { "bit-or",     bi_bit_or },
    { "bit-xor",    bi_bit_xor },
    { "bit-neg",    bi_bit_neg },
    { "<<",         bi_shift_left },
    { ">>",         bi_shift_right },

    { "peek",       bi_peek },
    { "poke",       bi_poke },
    { "sys",        bi_sys },

    { "read",       bi_read },
    { "print",      bi_print },
    { "princ",      bi_princ },

    { "fn",         bi_fn },
    { "var",        bi_var },
    { "gc",         bi_gc },

    { "exit",       bi_exit },

    { NULL, NULL }
};

void
init_builtins (void)
{
    return_tag = lisp_make_symbol ("%R", 2);
    go_tag = lisp_make_symbol ("%G", 2);
    EXPAND_UNIVERSE(return_tag);
    EXPAND_UNIVERSE(go_tag);
    add_builtins (builtins);
}

void
load_environment (void)
{
    lispptr x;

    outs ("\n\rLoading ENV.LISP...\n\r");
    cbm_open (3, 8, 3, "ENV.LISP");
    // TODO: Error check.
    cbm_k_chkin (3);
    while (x = lisp_read ()) {
        lisp_print (x);
        outs ("\n\r");
        x = eval (x);
        lisp_print (x);
        outs ("\n\r");
    }
    cbm_k_close (3);
    cbm_k_clrch ();
}

int
main (int argc, char * argv[])
{
    (void) argc, (void) argv;

    if (!lisp_init ())
        error ("No memory.");
    init_builtins ();
    load_environment ();

    return 0;
}
