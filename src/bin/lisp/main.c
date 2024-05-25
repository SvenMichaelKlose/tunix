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
extern lispptr x;
extern lispptr value;
lispptr arg1;
lispptr arg2c;
lispptr arg2;
lispptr stdin;
lispptr stdout;
lispptr lisp_fnin;
lispptr lisp_fnout;
char * msg;
lispptr quote;
lispptr return_tag;
lispptr go_tag;
extern lispptr tmp;
int len;
lispptr b;
lispptr tag;
bool    tag_found;
#ifdef __CC65__
#pragma zpsym ("x")
#pragma zpsym ("value")
#pragma zpsym ("arg1")
#pragma zpsym ("arg2c")
#pragma zpsym ("arg2")
#pragma zpsym ("stdin")
#pragma zpsym ("stdout")
#pragma zpsym ("lisp_fnin")
#pragma zpsym ("lisp_fnout")
#pragma zpsym ("msg")
#pragma zpsym ("quote")
#pragma zpsym ("return_tag")
#pragma zpsym ("go_tag")
#pragma zpsym ("tmp")
#pragma zpsym ("len")
#pragma zpsym ("b")
#pragma zpsym ("tag")
#pragma zpsym ("tag_found")
#pragma bss-name (pop)
#endif

lispptr go_expr;
lispptr return_expr;
lispptr return_args;

#ifdef SLOW

lispptr FASTCALL
lisp_car (lispptr x)
{
    return CONS(x)->car;
}

lispptr FASTCALL
lisp_cdr (lispptr x)
{
    return CONS(x)->cdr;
}

bool FASTCALL
lisp_consp (lispptr x)
{
    return TYPE(x) == TYPE_CONS;
}

#endif

char load_fn = 10;

extern void error (char * msg);

void
error (char * msg)
{
    errouts ("ERROR: ");
    outs (msg);
    terpri ();
    while (1);
}

void
bierror ()
{
    error (msg);
}

int
length (lispptr x)
{
    len = 0;
    for (; x; x = CDR(x))
        len++;
    return len;
}

void
name_to_buffer (lispptr s)
{
    uchar len;
    len = SYMBOL_LENGTH(s);
    memcpy (buffer, SYMBOL_NAME(s), len);
    buffer[len] = 0;
}

void
ensure_undefd_arg1 ()
{
    for (x = universe; x; x = CDR(x)) {
        if (CAR(x) == arg1) {
            lisp_print (arg1);
            msg = " already defined.";
            bierror ();
        }
    }
}

void
ensure_one_arg (void)
{
    if (!CONSP(x)
        || CDR(x))
        bierror ();
    arg1 = eval (CAR(x));
}

void
ensure_number_arg (void)
{
    ensure_one_arg ();
    if (!NUMBERP(arg1))
        bierror ();
}

void
ensure_symbol_arg (void)
{
    ensure_one_arg ();
    if (!SYMBOLP(arg1))
        bierror ();
}

void
ensure_two_args (void)
{
    if (!CONSP(x)
        || !CONSP(arg2c = CDR(x))
        || CDR(arg2c))
        bierror ();
    arg1 = eval (CAR(x));
    PUSH(arg1);
    arg2 = eval (CAR(arg2c));
    POP(arg1);
}

void
ensure_one_number (void)
{
    ensure_one_arg ();
    if (!NUMBERP(arg1))
        bierror ();
}

void
ensure_two_numbers (void)
{
    ensure_two_args ();
    if (!NUMBERP(arg1)
        || !NUMBERP(arg2))
        bierror ();
}

void
ensure_numbers (void)
{
    if (!CONSP(x)
        || !NUMBERP(arg1 = eval (CAR(x)))
        || !(arg2c = CDR(x)))
        bierror ();
}

void
cons_getter_args (void)
{
    ensure_one_arg ();
    if (!LISTP(arg1))
        bierror ();
}

void
cons_setter_args (void)
{
    ensure_two_args ();
    if (!CONSP(arg2))
        bierror ();
    POP(arg1);
}

lispptr
bi_eq (void)
{
    msg = "(eq x x)";
    ensure_two_args ();
    return arg1 == arg2 ? t : nil;
}

lispptr
bi_not (void)
{
    msg = "(not x)";
    ensure_one_arg ();
    return !arg1 ? t : nil;
}

lispptr
bi_atom (void)
{
    msg = "(atom x)";
    ensure_one_arg ();
    return CONSP(arg1) ? nil : t;
}

lispptr
bi_symbolp (void)
{
    msg = "(symbol? x)";
    ensure_one_arg ();
    return SYMBOLP(arg1) ? t : nil;
}

lispptr
bi_setq (void)
{
    if (!CONSP(x)
        || !CONSP(arg2c = LIST_CDR(x))
        || !SYMBOLP(arg1 = CAR(x))
        || CDR(arg2c)) {
        msg = "(setq sym x)";
        bierror ();
    }
    SET_SYMBOL_VALUE(arg1, arg2 = eval (CAR(arg2c)));
    return arg2;
}

lispptr
bi_symbol_value (void)
{
    msg = "(symbol-value symbol)";
    ensure_symbol_arg ();
    return SYMBOL_VALUE(arg1);
}

lispptr
bi_string (void)
{
    int len;
    lispptr s;
    char * p;

    msg = "(string nlst)";
    ensure_one_arg ();
    len = length (arg1);
    s = lisp_alloc_symbol (buffer, len);

    for (p = SYMBOL_NAME(s); arg1; arg1 = CDR(arg1)) {
        if (!NUMBERP(CAR(arg1))) {
            msg = "(string nlst)";
            bierror ();
        }
        *p++ = NUMBER_VALUE(CAR(arg1));
    }
    *p++ = 0;
    return s;
}

lispptr
bi_quote (void)
{
    if (!CONSP(x)
        || CDR(x)) {
        msg = "(quote x)";
        bierror ();
    }
    return CAR(x);
}

lispptr
bi_consp (void)
{
    msg = "(cons? x)";
    ensure_one_arg ();
    return CONSP(arg1) ? t : nil;
}

lispptr
bi_cons (void)
{
    msg = "(cons x x)";
    ensure_two_args ();
    return lisp_make_cons(arg1, arg2);
}

lispptr
bi_car (void)
{
    msg = "(car lst)";
    cons_getter_args ();
    return LIST_CAR(arg1);
}

lispptr
bi_cdr (void)
{
    msg = "(cdr lst)";
    cons_getter_args ();
    return LIST_CDR(arg1);
}

lispptr
bi_rplaca (void)
{
    msg = "(rplaca x c)";
    cons_setter_args ();
    return RPLACA(arg1, arg2);
}

lispptr
bi_rplacd (void)
{
    msg = "(rplacd x c)";
    cons_setter_args ();
    return RPLACD(arg1, arg2);
}

lispptr
bi_numberp (void)
{
    msg = "(number? x)";
    ensure_one_arg ();
    return NUMBERP(CAR(x)) ? t : nil;
}

lispptr
bi_equal (void)
{
    msg = "(== n n)";
    ensure_two_numbers ();
    return BOOL(NUMBER_VALUE(arg1) == NUMBER_VALUE(arg2));
}

lispptr
bi_lt (void)
{
    msg = "(< n n)";
    ensure_two_numbers ();
    return BOOL(NUMBER_VALUE(arg1) < NUMBER_VALUE(arg2));
}

lispptr
bi_lte (void)
{
    msg = "(<= n n)";
    ensure_two_numbers ();
    return BOOL(NUMBER_VALUE(arg1) <= NUMBER_VALUE(arg2));
}

lispptr
bi_gt (void)
{
    msg = "(> n n)";
    ensure_two_numbers ();
    return BOOL(NUMBER_VALUE(arg1) > NUMBER_VALUE(arg2));
}

lispptr
bi_gte (void)
{
    msg = "(>= n n)";
    ensure_two_numbers ();
    return BOOL(NUMBER_VALUE(arg1) >= NUMBER_VALUE(arg2));
}

#define DOLIST(x, init) \
    for (x = init; x; x = LIST_CDR(x))

#define DEFARITH(fun_name, op, err) \
lispptr \
fun_name (void) \
{ \
    int v; \
    msg = err; \
    ensure_numbers (); \
    v = NUMBER_VALUE(arg1); \
    DOLIST(x, arg2c) { \
        PUSH(x); \
        if (!NUMBERP(tmp = eval (CAR(x)))) \
            bierror (); \
        POP(x); \
        v op NUMBER_VALUE(tmp); \
    } \
    return lisp_make_number (v); \
}

DEFARITH(bi_add, +=, "(+ n n...)");
DEFARITH(bi_sub, -=, "(- n n...)");
DEFARITH(bi_mul, *=, "(* n n...)");
DEFARITH(bi_div, /=, "(/ n n...)");
DEFARITH(bi_mod, %=, "(% n n...)");

lispptr
bi_inc (void)
{
    msg = "(++ n)";
    ensure_one_number ();
    return lisp_make_number (NUMBER_VALUE(arg1) + 1);
}

lispptr
bi_dec (void)
{
    msg = "(-- n)";
    ensure_one_number ();
    return lisp_make_number (NUMBER_VALUE(arg1) - 1);
}

lispptr
bi_bit_and (void)
{
    msg = "(bit-and n n)";
    ensure_two_numbers ();
    return lisp_make_number (NUMBER_VALUE(arg1) & NUMBER_VALUE(arg2));
}

lispptr
bi_bit_or (void)
{
    msg = "(bit-or n n)";
    ensure_two_numbers ();
    return lisp_make_number (NUMBER_VALUE(arg1) | NUMBER_VALUE(arg2));
}

lispptr
bi_bit_xor (void)
{
    msg = "(bit-xor n n)";
    ensure_two_numbers ();
    return lisp_make_number (NUMBER_VALUE(arg1) ^ NUMBER_VALUE(arg2));
}

lispptr
bi_bit_neg (void)
{
    msg = "(bit-neg n)";
    ensure_one_number ();
    return lisp_make_number (~NUMBER_VALUE(arg1));
}

lispptr
bi_shift_left (void)
{
    msg = "(<< n nbits)";
    ensure_two_numbers ();
    return lisp_make_number (NUMBER_VALUE(arg1) << NUMBER_VALUE(arg2));
}

lispptr
bi_shift_right (void)
{
    msg = "(>> n nbits)";
    ensure_two_numbers ();
    return lisp_make_number (NUMBER_VALUE(arg1) >> NUMBER_VALUE(arg2));
}

lispptr
bi_peek (void)
{
    msg = "(peek addr)";
    ensure_one_number ();
    return lisp_make_number (*(char *) NUMBER_VALUE(arg1));
}

lispptr
bi_poke (void)
{
    msg = "(poke addr b)";
    ensure_two_numbers ();
    *(char *) NUMBER_VALUE(arg1) = NUMBER_VALUE(arg2);
    return arg2;
}

lispptr
bi_sys (void)
{
    msg = "(sys addr)";
    ensure_one_number ();
    ((void (*) (void)) NUMBER_VALUE(arg1)) ();
    return nil;
}

lispptr
bi_eval (void)
{
    msg = "(eval x)";
    ensure_one_arg ();
    return eval (arg1);
}

lispptr
bi_apply (void)
{
    if (!CONSP(x)
        || !(arg2c = CDR(x))
        || CDR(arg2c)) {
        msg = "(apply fun . args)";
        bierror ();
    }
    args = arg2c;
    return apply (CAR(x), true);
}

lispptr
bi_block (void)
{
    if (!CONSP(x) || !SYMBOLP(arg1 = CAR(x))) {
        msg = "(block name . exprs)";
        bierror ();
    }
    arg2c = CDR(x);

    DOLIST(b, arg2c) {
        PUSH(arg2c);
        PUSH(b);
        value = eval (CAR(b));
        POP(b);
        POP(arg2c);
        if (CONSP(value)) {
            tmp = CAR(value);

            // Handle GO.
            if (tmp == go_tag) {
                // Search tag in body.
                tag = CAR(CDR(value));
                value = nil;
                tag_found = false;
                for (b = arg2c; b; b = LIST_CDR(b)) {
                    if (CAR(b) == tag) {
                        tag_found = true;
                        break;
                    }
                }
                if (!tag_found)
                    error ("Tag not found.");
            }

            // Handle RETURN.
            if (tmp == return_tag) {
                if (arg1 == CAR(CDR(value)))
                    return CDR(CDR(value));
                return value;
            }
        }
    }
    return value;
}

lispptr
bi_return (void)
{
    if (!CONSP(x)) {
        msg = "(return x [name])";
        bierror ();
    }
    // TODO: Re-use list.
    arg1 = eval (CAR(x));
    PUSH(arg1);
    arg2 = eval (LIST_CAR(LIST_CDR(x)));
    POP(arg1);
    RPLACA(return_args, arg1);
    RPLACD(return_args, arg2);
    return return_expr;
}

lispptr
bi_go (void)
{
    msg = "(go tag)";
    ensure_one_arg ();
    RPLACD(arg1, go_expr);
    return go_expr;
}

lispptr
bi_if (void)
{
    if (!CONSP(x)
         || !CONSP(arg2c = CDR(x))) {
        msg = "(? cond x [cond x/default])";
        bierror ();
    }
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
}

lispptr
bi_and (void)
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

lispptr
bi_or (void)
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

lispptr
bi_read (void)
{
    if (x) {
        msg = "(read)";
        bierror ();
    }
    return lisp_read ();
}

lispptr
bi_print (void)
{
    msg = "(print x)";
    ensure_one_arg ();
    return lisp_print (arg1);
}

lispptr
bi_fn (void)
{
    if (!CONSP(x)
        || !SYMBOLP(arg1 = CAR(x))
        || !CONSP(arg2c = CDR(x))) {
        msg = "(fn name obj)";
        bierror ();
    }
    ensure_undefd_arg1 ();
    EXPAND_UNIVERSE(arg1);
    SET_SYMBOL_VALUE(arg1, arg2c);
    return nil;
}

lispptr
bi_var (void)
{
    if (!CONSP(x)
        || !SYMBOLP(arg1 = CAR(x))
        || !CONSP(arg2c = CDR(x))
        || CDR(arg2c)) {
        msg = "(var name obj)";
        bierror ();
    }
    ensure_undefd_arg1 ();
    EXPAND_UNIVERSE(arg1);
    PUSH(arg1);
    SET_SYMBOL_VALUE(arg1, eval (CAR(arg2c)));
    POP(arg1);
    return nil;
}

lispptr
bi_gc (void)
{
    gc ();
    return lisp_make_number (heap_end - heap_free);
}

lispptr
bi_exit (void)
{
    msg = "(exit n)";
    ensure_one_number ();
    while (1);
    exit (NUMBER_VALUE(arg1));
    /* NOTREACHED */
    return nil;
}

lispptr
bi_err (void)
{
    if (x) {
        msg = "(err)";
        bierror ();
    }
    return lisp_make_number (err ());
}

lispptr
bi_eof (void)
{
    if (x) {
        msg = "(eof)";
        bierror ();
    }
    return BOOL(eof ());
}

lispptr
bi_open (void)
{
    uchar fn;
    msg = "(open fn s)";
    ensure_two_args ();
    if (!NUMBERP(arg1)) {
        msg = "(open fn s)";
        bierror ();
    }
    if (!SYMBOLP(arg2)) {
        msg = "(open fn s)";
        bierror ();
    }
    fn = NUMBER_VALUE(arg1);
    name_to_buffer (arg2);
    cbm_open (fn, 8, fn, buffer);
    return lisp_make_number (err ());
}

lispptr
bi_setin (void)
{
    msg = "(setin fn)";
    ensure_number_arg ();
    setin (NUMBER_VALUE(arg1));
    SET_SYMBOL_VALUE(lisp_fnin, arg1);
    return arg1;
}

lispptr
bi_setout (void)
{
    msg = "(setout fn)";
    ensure_number_arg ();
    setout (NUMBER_VALUE(arg1));
    if (err ())
        error ("setout: illegal fn.");
    SET_SYMBOL_VALUE(lisp_fnout, arg1);
    return arg1;
}

lispptr
bi_in (void)
{
    if (x) {
        msg = "(in)";
        bierror ();
    }
    return lisp_make_number (in ());
}

lispptr
bi_putback (void)
{
    if (x) {
        msg = "(putback)";
        bierror ();
    }
    putback ();
    return nil;
}

lispptr
bi_out (void)
{
    msg = "(out n/s)";
    ensure_one_arg ();
    if (NUMBERP(arg1))
        out (NUMBER_VALUE(arg1));
    else if (SYMBOLP(arg1))
        outsn (SYMBOL_NAME(arg1), SYMBOL_LENGTH(arg1));
    else
        lisp_print (arg1);
    return arg1;
}

lispptr
bi_terpri (void)
{
    if (x) {
        msg = "(terpri)";
        bierror ();
    }
    terpri ();
    return nil;
}

lispptr
bi_close (void)
{
    msg = "(close fn)";
    ensure_number_arg ();
    cbm_k_clrch ();
    cbm_k_close (NUMBER_VALUE(arg1));
    return nil;
}

void
load (char * pathname)
{
    int oldin = fnin;

    cbm_open (load_fn, 8, load_fn, pathname);
    if (err ()) {
        errouts ("Cannot open file ");
        error (pathname);
    }
    x = lisp_make_cons (lisp_make_number (load_fn), nil);
    bi_setin ();
    load_fn++;

    while (x = lisp_read ())
        eval (x);

    load_fn--;
    cbm_k_clrch ();
    cbm_k_close (load_fn);
    x = lisp_make_cons (lisp_make_number (oldin), nil);
    bi_setin ();
}

lispptr
bi_load (void)
{
    msg = "(load s)";
    ensure_symbol_arg ();
    name_to_buffer (arg1);
    load (buffer);
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
    { "open",       bi_open },
    { "err",        bi_err },
    { "eof",        bi_eof },
    { "in",         bi_in },
    { "out",        bi_out },
    { "terpri",     bi_terpri },
    { "setin",      bi_setin },
    { "setout",     bi_setout },
    { "putback",    bi_putback },
    { "close",      bi_close },
    { "load",       bi_load },

    { "fn",         bi_fn },
    { "var",        bi_var },
    { "gc",         bi_gc },

    { "exit",       bi_exit },

    { NULL, NULL }
};

void
init_builtins (void)
{
    return_tag  = lisp_make_symbol ("%R", 2);
    return_args = lisp_make_cons (nil, nil);
    return_expr = lisp_make_cons (return_tag, return_args);
    EXPAND_UNIVERSE(return_expr);

    go_tag  = lisp_make_symbol ("%G", 2);
    go_expr = lisp_make_cons (go_tag, nil);
    EXPAND_UNIVERSE(go_expr);

    add_builtins (builtins);
}

int
main (int argc, char * argv[])
{
    lispptr i;
    lispptr o;
    (void) argc, (void) argv;

    if (!lisp_init ())
        error ("No memory.");

    init_builtins ();

    // Prepare QUOTE.
    quote      = lisp_make_symbol ("quote", 5);
    EXPAND_UNIVERSE(quote);

    // Set up I/O streams.
    stdin      = lisp_make_symbol ("stdin", 5);
    stdout     = lisp_make_symbol ("stdout", 6);
    lisp_fnin  = lisp_make_symbol ("fnin", 4);
    lisp_fnout = lisp_make_symbol ("fnout", 5);
    fnin  = STDIN;
    fnout = STDOUT;
    i = lisp_make_number (STDIN);
    o = lisp_make_number (STDOUT);
    SET_SYMBOL_VALUE(stdin, i);
    SET_SYMBOL_VALUE(stdout, o);
    SET_SYMBOL_VALUE(lisp_fnin, i);
    SET_SYMBOL_VALUE(lisp_fnout, o);
    EXPAND_UNIVERSE(stdin);
    EXPAND_UNIVERSE(stdout);
    EXPAND_UNIVERSE(lisp_fnin);
    EXPAND_UNIVERSE(lisp_fnout);

    load ("ENV.LISP");
    while (0) {
        outs ("* ");
        lisp_print (eval (lisp_read ()));
        terpri ();
    }

    while (1);
    return 0;
}
