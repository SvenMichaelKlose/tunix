#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#include <cbm.h>
#endif

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
lispptr arg2;
lispptr arg2c;
lispptr argrest;
lispptr lisp_fnin;
lispptr lisp_fnout;
char * msg;
lispptr quote;
lispptr return_sym;
lispptr return_name;
lispptr return_value;
lispptr go_sym;
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
#pragma zpsym ("lisp_fnin")
#pragma zpsym ("lisp_fnout")
#pragma zpsym ("msg")
#pragma zpsym ("quote")
#pragma zpsym ("return_sym")
#pragma zpsym ("return_name")
#pragma zpsym ("return_value")
#pragma zpsym ("go_sym")
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

lispptr start;
lispptr lastc;

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

void
error (char * msg)
{
    lisp_break = true;
    setout (STDERR);
    outs ("ERROR: ");
    if (msg) {
        outs (msg);
        msg = NULL;
    }
    lisp_print (x);
    terpri ();
    lisp_repl ();
}

void
bierror ()
{
    error (msg);
while (1);
}

void
name_to_buffer (lispptr s)
{
    uchar len;
    len = SYMBOL_LENGTH(s);
    memcpy (buffer, SYMBOL_NAME(s), len);
    buffer[len] = 0;
}

int
length (lispptr x)
{
    len = 0;
    DOLIST(x, x)
        len++;
    return len;
}

lispptr
butlast (lispptr x)
{
    if (LIST_CDR(x))
        return lisp_make_cons (CAR(x), butlast (CDR(x)));
    return nil;
}

lispptr
last (lispptr x)
{
    return LIST_CDR(x) ? last (CDR(x)) : x;
}

void
ensure_undefd_arg1 ()
{
    DOLIST(x, universe) {
        if (CAR(x) == arg1) {
            msg = "Already defined:";
            bierror ();
            lisp_print (arg1);
        }
    }
}

void
ensure_one_arg (void)
{
    if (!CONSP(x)
        || CDR(x))
        bierror ();
    x = CAR(x);
    arg1 = eval ();
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
    if (!CONSP(x))
        bierror ();
    arg2c = CDR(x);
    if (!CONSP(arg2c)
         || CDR(arg2c))
        bierror ();

    PUSH(arg2c);
    x = CAR(x);
    arg1 = eval ();
    POP(arg2c);

    PUSH(arg1);
    x = CAR(arg2c);
    arg2 = eval ();
    POP(arg1);
}

void
ensure_number (void)
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
ensure_first_is_number (void)
{
    if (!CONSP(x)
        || !(arg2c = CDR(x)))
        bierror ();
    x = CAR(x);
    PUSH(arg2c);
    arg1 = eval ();
    POP(arg2c);
    if (!NUMBERP(arg1))
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
    if (CONSP(arg2))
        POP(arg1);
    else
        bierror ();
}

lispptr
bi_eq (void)
{
    msg = "(eq x x)";
    ensure_two_args ();
    return BOOL(arg1 == arg2);
}

lispptr
bi_not (void)
{
    msg = "(not x)";
    ensure_one_arg ();
    return arg1 ? nil : t;
}

lispptr
bi_atom (void)
{
    msg = "(atom x)";
    ensure_one_arg ();
    return CONSP(arg1) ? nil : arg1;
}

lispptr
bi_symbolp (void)
{
    if (!arg1)
        return t;
    return SYMBOLP(arg1) ? arg1 : nil;
}

lispptr
bi_setq (void)
{
    if (!CONSP(x))
        bierror ();
    arg2c = LIST_CDR(x);
    if (!CONSP(arg2c))
        bierror ();
    arg1 = CAR(x);
    if (!SYMBOLP(arg1)
        || CDR(arg2c)) {
        msg = "(setq sym x)";
        bierror ();
    }
    x = CAR(arg2c);
    PUSH(arg1);
    arg2 = eval ();
    POP(arg1);
    SET_SYMBOL_VALUE(arg1, arg2);
    return arg2;
}

lispptr
bi_symbol_value (void)
{
    msg = "(symbol-value symbol)";
    ensure_symbol_arg ();
    return SYMBOL_VALUE(arg1);
}

// Make char list from symbol name.
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
    return arg1;
}

lispptr
bi_consp (void)
{
    return BOOL(CONSP(arg1));
}

lispptr
bi_cons (void)
{
    msg = "(cons x x)";
    ensure_two_args ();
    return lisp_make_cons (arg1, arg2);
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
bi_setcar (void)
{
    msg = "(setcar x c)";
    cons_setter_args ();
    return SETCAR(arg1, arg2);
}

lispptr
bi_setcdr (void)
{
    msg = "(setcdr x c)";
    cons_setter_args ();
    return SETCDR(arg1, arg2);
}

lispptr
bi_numberp (void)
{
    return NUMBERP(arg1) ? arg1 : nil;
}

lispptr
bi_number_equal (void)
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

#define DEFARITH(fun_name, op, err) \
lispptr \
fun_name (void) \
{ \
    return lisp_make_number (NUMBER_VALUE(arg1) op NUMBER_VALUE(arg2)); \
}

DEFARITH(bi_add, +, "(+ n n)");
DEFARITH(bi_sub, -, "(- n n)");
DEFARITH(bi_mul, *, "(* n n)");
DEFARITH(bi_div, /, "(/ n n)");
DEFARITH(bi_mod, %, "(% n n)");

lispptr
bi_inc (void)
{
    msg = "(++ n)";
    ensure_number ();
    return lisp_make_number (NUMBER_VALUE(arg1) + 1);
}

lispptr
bi_dec (void)
{
    msg = "(-- n)";
    ensure_number ();
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
    ensure_number ();
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
    ensure_number ();
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
    ensure_number ();
    ((void (*) (void)) NUMBER_VALUE(arg1)) ();
    return nil;
}

lispptr
bi_eval (void)
{
    x = arg1;
    return eval ();
}

lispptr
bi_apply (void)
{
    if (!CONSP(x)) {
        msg = "(apply fun . args)";
        bierror ();
    }
    arg1 = CAR(x);
    arg2c = CDR(x);
    // Consing. Could be optimized away if moved to eval().
    x = butlast (arg2c);
    PUSH(arg1);
    PUSH(arg2c);
    args = eval_list ();
    POP(arg2c);
    POP(arg1);
    x = last (arg2c);
    PUSH(arg1);
    PUSH(args);
    tmp = eval_list ();
    POP(args);
    POP(arg1);
    if (args) {
        if (!LISTP(tmp)) {
            msg = "Last argument must be a list.";
            bierror ();
        }
        SETCDR(last (args), CAR(tmp));
    } else
        args = CAR(tmp);
    x = lisp_make_cons (arg1, args);
    return funcall ();
}

lispptr
bi_block (void)
{
    msg = "(block name . exprs)";
    if (!CONSP(x))
        bierror ();
    arg1 = CAR(x);
    if (!SYMBOLP(arg1)) {
        msg = "(block name . exprs)";
        bierror ();
    }
    arg2c = CDR(x);

    DOLIST(b, arg2c) {
        if (lisp_break)
            return nil;

        PUSH(arg1);
        PUSH(arg2c);
        PUSH(b);
        x = CAR(b);
        value = eval ();
        POP(b);
        POP(arg2c);
        POP(arg1);

        // Handle GO.
        if (value == go_sym) {
            // Search tag in body.
            value = nil;
            tag_found = false;
            TYPESAFE_DOLIST(b, arg2c) {
                if (CAR(b) == go_tag) {
                    tag_found = true;
                    break;
                }
            }
            if (!tag_found) {
                error ("Tag not found.");
                return nil;
            }
        }

        // Handle RETURN.
        if (value == return_sym) {
            if (arg1 == return_name) {
                tmp = return_value;
                return_value = nil;
                return tmp;
            }
            return value;
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
    PUSH(x);
    x = CAR(x);
    return_value = eval ();
    POP(x);
    x = LIST_CAR(LIST_CDR(x));
    return_name = eval ();
    return return_sym;
}

lispptr
bi_go (void)
{
    go_tag = arg1;
    return go_sym;
}

lispptr
bi_if (void)
{
    msg = "(? cond x [cond x/default])";
    if (!CONSP(x))
        bierror ();
    arg2c = CDR(x);
    if (!CONSP(arg2c))
        bierror ();
    while (x) {
        arg1 = CAR(x);
        if (!(arg2c = CDR(x))) {
            x = arg1;
            return delayed_eval;
        }

        PUSH(arg2c);
        x = arg1;
        tmp = eval ();
        POP(arg2c);
        if (tmp) {
            x = CAR(arg2c);
            return delayed_eval;
        }
        x = CDR(arg2c);
    }
    /* NOTREACHED */
}

lispptr
bi_and (void)
{
    value = nil;
    DOLIST(x, x) {
        PUSH(x);
        x = CAR(x);
        value = eval ();
        POP(x);
        if (!value)
            return nil;
    }
    return value;
}

lispptr
bi_or (void)
{
    DOLIST(x, x) {
        PUSH(x);
        x = CAR(x);
        value = eval ();
        POP(x);
        if (value)
            return value;
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
    return lisp_print (arg1);
}

lispptr
bi_err (void)
{
    return lisp_make_number (err ());
}

lispptr
bi_eof (void)
{
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
    simpleio_open (fn, buffer, 'r');
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
    return lisp_make_number (in ());
}

lispptr
bi_putback (void)
{
    putback ();
    return nil;
}

lispptr
bi_out (void)
{
    msg = "(out n/s)";
    ensure_one_arg ();
    if (!arg1)
        outs ("nil");
    else if (NUMBERP(arg1))
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
    terpri ();
    return nil;
}

lispptr
bi_close (void)
{
    msg = "(close fn)";
    ensure_number_arg ();
    simpleio_close (NUMBER_VALUE(arg1));
    return nil;
}

void
load (char * pathname)
{
    int oldin = fnin;

    simpleio_open (load_fn, pathname, 'r');
    if (err ()) {
        errouts ("Cannot open file ");
        error (pathname);
    }
    x = lisp_make_cons (lisp_make_number (load_fn), nil);
    bi_setin ();
    load_fn++;

    while (!lisp_break && (x = lisp_read ()))
        eval ();

    load_fn--;
    simpleio_close (load_fn);
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

lispptr
bi_fn (void)
{
    msg = "(fn name obj)";
    if (!CONSP(x))
        bierror ();
    arg1 = CAR(x);
    if (!SYMBOLP(arg1))
        bierror ();
    arg2c = CDR(x);
    if (!CONSP(arg2c))
        bierror ();
    ensure_undefd_arg1 ();
    EXPAND_UNIVERSE(arg1);
    SET_SYMBOL_VALUE(arg1, arg2c);
    return nil;
}

lispptr
bi_var (void)
{
    msg = "(var name obj)";
    if (!CONSP(x))
        bierror ();
    arg1 = CAR(x);
    if (!SYMBOLP(arg1))
        bierror ();
    arg2c = CDR(x);
    if (!CONSP(arg2c)
        || CDR(arg2c))
        bierror ();
    ensure_undefd_arg1 ();
    EXPAND_UNIVERSE(arg1);
    PUSH(arg1);
    PUSH(arg2c);
    x = CAR(arg2c);
    SET_SYMBOL_VALUE(arg1, eval ());
    POP(arg2c);
    POP(arg1);
    return nil;
}

lispptr
bi_universe (void)
{
    return universe;
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
    ensure_number ();
#ifdef __CC65__
    while (1);
#endif
    exit (NUMBER_VALUE(arg1));
    /* NOTREACHED */
    return nil;
}

lispptr
bi_length (void)
{
    msg = "(length l)";
    ensure_one_arg ();
    return lisp_make_number (length (arg1));
}

lispptr
bi_butlast (void)
{
    msg = "(butlast l)";
    ensure_one_arg ();
    return butlast (arg1);
}

lispptr
bi_last (void)
{
    msg = "(last l)";
    ensure_one_arg ();
    return last (arg1);
}

lispptr
bi_filter (void)
{
    msg = "(@ f l)";
    ensure_two_args ();
    if (!arg2) {
        bierror ();
        return nil;
    }
    PUSH(arg1);
    PUSH(arg2);
    x = lisp_make_cons (arg1, lisp_make_cons (CAR(arg2), nil));
    start = lastc = lisp_make_cons (eval (), nil);
    POP(arg2);
    POP(arg1);
    PUSH(start);
    DOLIST(arg2, CDR(arg2)) {
        PUSH(arg1);
        PUSH(arg2);
        PUSH(lastc);
        x = lisp_make_cons (arg1, lisp_make_cons (CAR(arg2), nil));
        tmp = lisp_make_cons (eval (), nil);
        POP(lastc);
        SETCDR(last, tmp);
        lastc = tmp;
        POP(arg2);
        POP(arg1);
    }
    POP(start);
    return start;
}

struct builtin builtins[] = {
    { "quote",      "'x", bi_quote },

    { "apply",      NULL, bi_apply },
    { "eval",       "x", bi_eval },

    { "?",          NULL, bi_if },
    { "and",        NULL, bi_and },
    { "or",         NULL, bi_or },
    { "block",      NULL, bi_block },
    { "return",     NULL, bi_return },
    { "go",         "'x", bi_go },

    { "not",        NULL, bi_not },
    { "eq",         NULL, bi_eq },
    { "atom",       NULL, bi_atom },
    { "cons?",      "x", bi_consp },
    { "number?",    "x", bi_numberp },
    { "symbol?",    "x", bi_symbolp },

    { "=",          NULL, bi_setq },
    { "value",      NULL, bi_symbol_value },
    { "string",     NULL, bi_string },

    { "cons",       NULL, bi_cons },
    { "car",        NULL, bi_car },
    { "cdr",        NULL, bi_cdr },
    { "setcar",     NULL, bi_setcar },
    { "setcdr",     NULL, bi_setcdr },

    { "==",         NULL, bi_number_equal },
    { ">",          NULL, bi_gt },
    { "<",          NULL, bi_lt },
    { ">=",         NULL, bi_gte },
    { "<=",         NULL, bi_lte },

    { "+",          "nn", bi_add },
    { "-",          "nn", bi_sub },
    { "*",          "nn", bi_mul },
    { "/",          "nn", bi_div },
    { "%",          "nn", bi_mod },
    { "++",         NULL, bi_inc },
    { "--",         NULL, bi_dec },

    { "bit-and",    NULL, bi_bit_and },
    { "bit-or",     NULL, bi_bit_or },
    { "bit-xor",    NULL, bi_bit_xor },
    { "bit-neg",    NULL, bi_bit_neg },
    { "<<",         NULL, bi_shift_left },
    { ">>",         NULL, bi_shift_right },

    { "peek",       NULL, bi_peek },
    { "poke",       NULL, bi_poke },
    { "sys",        NULL, bi_sys },

    { "read",       NULL, bi_read },
    { "print",      "x", bi_print },
    { "open",       NULL, bi_open },
    { "err",        "", bi_err },
    { "eof",        "", bi_eof },
    { "in",         "", bi_in },
    { "out",        NULL, bi_out },
    { "terpri",     "", bi_terpri },
    { "setin",      NULL, bi_setin },
    { "setout",     NULL, bi_setout },
    { "putback",    "", bi_putback },
    { "close",      NULL, bi_close },
    { "load",       NULL, bi_load },

    { "fn",         NULL, bi_fn },
    { "var",        NULL, bi_var },
    { "universe",   "", bi_universe },
    { "gc",         "", bi_gc },
    { "exit",       NULL, bi_exit },

    { "@",          NULL, bi_filter },
    { "butlast",    NULL, bi_butlast },
    { "last",       NULL, bi_last },
    { "length",     NULL, bi_length },

    { NULL, NULL }
};

void
init_builtins (void)
{
    return_sym  = lisp_make_symbol (NULL, 0);
    go_sym      = lisp_make_symbol (NULL, 0);
    EXPAND_UNIVERSE(return_sym);
    EXPAND_UNIVERSE(go_sym);
    add_builtins (builtins);
}

lispptr
lisp_repl ()
{
    while (!eof ()) {
        lisp_break = false;
        outs ("* ");
        x = lisp_read ();
        fresh_line ();
        x = eval ();
        if (lisp_break)
            return x;
        fresh_line ();
        lisp_print (x);
        fresh_line ();
    }
    return x;
}

int
main (int argc, char * argv[])
{
    lispptr i;
    lispptr o;
    lispptr stdin;
    lispptr stdout;
    (void) argc, (void) argv;

    simpleio_init ();
    if (!lisp_init ())
        error ("No memory.");

    init_builtins ();

    // Prepare QUOTE.
    quote      = lisp_make_symbol ("quote", 5);
    EXPAND_UNIVERSE(quote);

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

    load ("env.lisp");
    lisp_repl ();

    return 0;
}
