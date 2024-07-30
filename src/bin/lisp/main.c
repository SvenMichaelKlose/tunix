#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#include <cbm.h>
#endif

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#ifndef __CC65__
#include <signal.h>
#endif

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

extern void test (void);

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern lispptr x;
extern lispptr args;
extern lispptr arg1;
extern lispptr arg2c;
extern lispptr arg2;
extern lispptr value;
extern lispptr tmp;
#ifdef __CC65__
#pragma zpsym ("x")
#pragma zpsym ("args")
#pragma zpsym ("arg1")
#pragma zpsym ("arg2c")
#pragma zpsym ("arg2")
#pragma zpsym ("value")
#pragma zpsym ("tmp")
#pragma bss-name (pop)
#endif

// I/O channels
lispptr lisp_fnin;
lispptr lisp_fnout;

// Symbols for quoting.
lispptr quote;
lispptr quasiquote;
lispptr unquote;
lispptr unquote_spliced;

// Set by DEBUG (bi_debug()).
// Used as breakpoint condition for host debugger.
#ifndef NDEBUG
bool debug_mode;
#endif

lispptr
bi_eq (void)
{
    return BOOL(arg1 == arg2);
}

lispptr
bi_not (void)
{
    return BOOL(!arg1);
}

lispptr
bi_atom (void)
{
    if (arg1)
        return ATOM(arg1) ? arg1 : nil;
    return t;
}

lispptr
bi_symbolp (void)
{
    if (arg1)
        return SYMBOLP(arg1) ? arg1 : nil;
    return t;
}

lispptr
bi_builtinp (void)
{
    if (arg1)
        return BUILTINP(arg1) ? arg1 : nil;
    return nil;
}

lispptr
bi_specialp (void)
{
    if (arg1)
        return SPECIALP(arg1) ? arg1 : nil;
    return nil;
}

lispptr
bi_setq (void)
{
    SET_SYMBOL_VALUE(arg1, arg2);
    return arg2;
}

lispptr
bi_symbol_value (void)
{
    return SYMBOL_VALUE(arg1);
}

// Make symbol from character list.
lispptr
bi_symbol (void)
{
    int len;
    lispptr s;
    char * p;

    // Get length.  Truncate at 255.
    arg1 = x ? CAR(x) : nil;
    len = length (arg1);
    if (len > 255)
        len = 255;

    // Allocate empty symbol of wanted length.
    PUSH(arg1);
    s = alloc_symbol (buffer, len);
    POP(arg1);

    // Make symbol name from list.
    p = SYMBOL_NAME(s);
    DOLIST(arg1, arg1) {
#ifndef NAIVE
        if (!NUMBERP(CAR(arg1))) {
            error (ERROR_TYPE, "(string nlst)");
            break;
        }
#endif
        *p++ = NUMBER_VALUE(CAR(arg1));
    }

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
    return make_cons (arg1, arg2);
}

lispptr
bi_car (void)
{
    return LIST_CAR(arg1);
}

lispptr
bi_cdr (void)
{
    return LIST_CDR(arg1);
}

lispptr
bi_setcar (void)
{
    return SETCAR(arg1, arg2);
}

lispptr
bi_setcdr (void)
{
    return SETCDR(arg1, arg2);
}

lispptr
bi_numberp (void)
{
    return NUMBERP(arg1) ? arg1 : nil;
}

#define DEFCOND(fun_name, op) \
lispptr \
fun_name (void) \
{ \
    return BOOL(NUMBER_VALUE(arg1) op NUMBER_VALUE(arg2)); \
}

DEFCOND(bi_number_equal, ==);
DEFCOND(bi_lt, <);
DEFCOND(bi_lte, <=);
DEFCOND(bi_gt, >);
DEFCOND(bi_gte, >=);

#define DEFOP(fun_name, op) \
lispptr \
fun_name (void) \
{ \
    return make_number (NUMBER_VALUE(arg1) op NUMBER_VALUE(arg2)); \
}

DEFOP(bi_add, +);
DEFOP(bi_sub, -);
DEFOP(bi_mul, *);
DEFOP(bi_div, /);
DEFOP(bi_mod, %);
DEFOP(bi_bit_and, &);
DEFOP(bi_bit_or, |);
DEFOP(bi_bit_xor, ^);
DEFOP(bi_shift_left, <<);
DEFOP(bi_shift_right, >>);

lispptr
bi_inc (void)
{
    return make_number (NUMBER_VALUE(arg1) + 1);
}

lispptr
bi_dec (void)
{
    return make_number (NUMBER_VALUE(arg1) - 1);
}

lispptr
bi_bit_neg (void)
{
    return make_number (~NUMBER_VALUE(arg1));
}

lispptr
bi_rawptr (void)
{
    return make_number ((lispnum_t) arg1);
}

lispptr
bi_peek (void)
{
    return make_number (*(char *) NUMBER_VALUE(arg1));
}

lispptr
bi_poke (void)
{
    *(char *) NUMBER_VALUE(arg1) = NUMBER_VALUE(arg2);
    return arg2;
}

lispptr
bi_sys (void)
{
    ((void (*) (void)) NUMBER_VALUE(arg1)) ();
    return nil;
}

lispptr
bi_eval (void)
{
    x = arg1;
    PUSH_TAG(TAG_DONE); // Tell to return from eval0().
    return eval0 ();
}

// Consing. Could be optimized away if moved to eval().
lispptr
bi_apply (void)
{
    PUSH(arg1);

    PUSH(arg2);
    args = copy_list (arg2, COPY_BUTLAST, nil);
    POP(arg2);
    tmp = LIST_CAR(last (arg2));

    if (args) {
#ifndef NAIVE
        if (!LISTP(tmp)) {
            error (ERROR_TYPE, "Last arg isn't list!");
            POP(arg1);
            return nil;
        }
#endif
        SETCDR(last (args), tmp);
    } else
        args = tmp;

    POP(arg1);
    make_call (args);
    return eval0 ();
}

lispptr
bi_funcall (void)
{
    make_call (arg2);
    return eval0 ();
}

lispptr
bi_return (void)
{
    return_value = arg1;
    return_name = arg2;
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
    arg2c = CDR(x);
    while (x) {
        arg1 = CAR(x);
        if (!(arg2c = CDR(x))) {
            x = arg1;
            return delayed_eval;
        }

        highlighted = arg2c;
        PUSH(arg2c);
        x = arg1;
        tmp = eval ();
        POP(arg2c);
        highlighted = arg2c;
#ifndef NAIVE
        if (error_code)
            break;
#endif
        if (tmp) {
            x = CAR(arg2c);
            return delayed_eval;
        }
        x = CDR(arg2c);
    }
    return nil;
}

lispptr
bi_and (void)
{
    value = nil;
    DOLIST(x, x) {
        PUSH(x);
        PUSH_HIGHLIGHTED(x);
        x = CAR(x);
        value = eval ();
        POP_HIGHLIGHTED();
        POP(x);
#ifndef NAIVE
        if (error_code)
            return nil;
#endif
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
        PUSH_HIGHLIGHTED(x);
        x = CAR(x);
        value = eval ();
        POP_HIGHLIGHTED();
        POP(x);
#ifndef NAIVE
        if (error_code)
            break;
#endif
        if (value)
            return value;
    }
    return nil;
}

lispptr
bi_print (void)
{
    return print (arg1);
}

// Get last I/O error.
// TODO: Rename to IOERR?
lispptr
bi_err (void)
{
    if (err ())
        return make_number (err ());
    return nil;
}

lispptr
bi_eof (void)
{
    return BOOL(eof ());
}

lispptr
bi_open (void)
{
    uchar fn = NUMBER_VALUE(arg1);
    name_to_buffer (arg2);
    simpleio_open (fn, buffer, 'r');
    return make_number (err ());
}

lispptr
bi_setin (void)
{
    setin (NUMBER_VALUE(arg1));
    SET_SYMBOL_VALUE(lisp_fnin, arg1);
    return arg1;
}

lispptr
bi_setout (void)
{
    setout (NUMBER_VALUE(arg1));
    SET_SYMBOL_VALUE(lisp_fnout, arg1);
    return arg1;
}

lispptr
bi_in (void)
{
    return make_number (in ());
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
    if (!arg1)
        outs ("nil");
    else if (NUMBERP(arg1))
        out (NUMBER_VALUE(arg1));
    else if (SYMBOLP(arg1))
        outsn (SYMBOL_NAME(arg1), SYMBOL_LENGTH(arg1));
    else
        print (arg1);
    return arg1;
}

lispptr
bi_terpri (void)
{
    terpri ();
    return nil;
}

lispptr
bi_fresh_line (void)
{
    fresh_line ();
    return nil;
}

lispptr
bi_close (void)
{
    simpleio_close (NUMBER_VALUE(arg1));
    return nil;
}

lispptr
bi_load (void)
{
    outs ("Loading "); print (arg1); terpri ();
    name_to_buffer (arg1);
    load (buffer);
    return nil;
}

lispptr
bi_define (void)
{
    if (member (arg1, universe))
        outs ("Redefining ");
    else {
        expand_universe (arg1);
        outs ("Defining ");
    }
    print (arg1);
    terpri ();
    SET_SYMBOL_VALUE(arg1, arg2);
    return arg1;
}

lispptr
bi_special (void)
{
    tmp = bi_define ();
    TYPE(tmp) = TYPE_SPECIAL;
    return tmp;
}

lispptr
bi_undef (void)
{
#ifndef NAIVE
    if (!arg1)
        error (ERROR_TYPE, "non-NIL expected");
#endif
    outs ("Undefining "); print (arg1); terpri ();
    PUSH(arg1);
    universe = copy_list (universe, COPY_REMOVE, arg1);
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
#ifdef FRAGMENTED_HEAP
    struct heap_fragment * h;
    size_t freed = 0;
#endif

    gc ();
#ifdef FRAGMENTED_HEAP
    for (h = heaps; h->start; h++)
        freed += h->end - h->free;
    return make_number (freed);
#else
    return make_number (heap_end - heap_free);
#endif
}

#ifndef NAIVE

lispptr
bi_error (void)
{
    last_errstr = "User error";
    if (arg1)
        current_expr = arg1;
    error_code = ERROR_USER;
    return nil;
}

lispptr
bi_ignore (void)
{
    do_break_repl = do_continue_repl = true;
    return nil;
}

lispptr
bi_stack (void)
{
    int i = 0;
    int old_out = fnout;
    lispptr * p;
    setout (STDERR);
    for (p = (void *) stack_end, p--; p != (void *) stack; p--) {
        outn (i++);
        outs (": ");
        print (*p);
        terpri ();
    }
    setout (old_out);
    return nil;
}
#endif // #ifndef NAIVE

lispptr
bi_quit (void)
{
    do_break_repl = true;
    return arg1;
}

lispptr
bi_exit (void)
{
    if (arg1)
        exit (NUMBER_VALUE(arg1));
    else
        do_exit_program = do_break_repl = true;
    return nil;
}

lispptr
bi_length (void)
{
    return make_number (length (arg1));
}

lispptr
bi_butlast (void)
{
    return copy_list (arg1, COPY_BUTLAST, nil);
}

lispptr
bi_copy_list (void)
{
    return copy_list (arg1, COPY_LIST, nil);
}

lispptr
bi_last (void)
{
    return last (arg1);
}

lispptr
bi_member (void)
{
    return member (arg1, arg2);
}

lispptr
bi_remove (void)
{
    return copy_list (arg2, COPY_REMOVE, arg1);
}

lispptr
bi_filter (void)
{
    PUSH(arg1);
    PUSH(arg2);
    make_car_call ();
    PUSH_HIGHLIGHTED(x);
    list_start = list_last = make_cons (eval0 (), nil);
    POP_HIGHLIGHTED();
    POP(arg2);
    POP(arg1);
    if (do_break_repl)
        return nil;

    PUSH(list_start);
    DOLIST(arg2, CDR(arg2)) {
        PUSH(arg1);
        PUSH(arg2);
        PUSH(list_last);
        make_car_call ();
        PUSH_HIGHLIGHTED(x);
        tmp = make_cons (eval0 (), nil);
        POP_HIGHLIGHTED();
        if (do_break_repl) {
            stack += 4 * sizeof (lispptr);
            return nil;
        }
        POP(list_last);
        SETCDR(list_last, tmp);
        list_last = tmp;
        POP(arg2);
        POP(arg1);
    }
    if (arg2) {
        PUSH(list_last);
        make_call (make_cons (arg2, nil));
        PUSH_HIGHLIGHTED(x);
        tmp = eval0 ();
        POP_HIGHLIGHTED();
        if (do_break_repl) {
            stack += 2 * sizeof (lispptr);
            return nil;
        }
        POP(list_last);
        SETCDR(list_last, tmp);
    }
    POP(list_start);
    return list_start;
}

#ifndef NDEBUG
lispptr
bi_debug (void)
{
    debug_mode = true;
#ifndef __CC65__
    raise (SIGTRAP);
#endif
    return nil;
}
#endif

#ifndef NO_DEBUGGER
lispptr
bi_debugger (void)
{
    debug_step = t;
    return nil;
}
#endif

struct builtin builtins[] = {
    { "quote",      "'x",   bi_quote },

    { "apply",      "f+x",  bi_apply },
    { "funcall",    "f+x",  bi_funcall },
    { "eval",       "x",    bi_eval },

    { "?",          NULL,   bi_if },
    { "and",        NULL,   bi_and },
    { "or",         NULL,   bi_or },
    { "return",     "x?s",  bi_return },
    { "go",         "'x",   bi_go },

    { "not",        "x",    bi_not },
    { "eq",         "xx",   bi_eq },
    { "atom",       "x",    bi_atom },
    { "cons?",      "x",    bi_consp },
    { "number?",    "x",    bi_numberp },
    { "symbol?",    "x",    bi_symbolp },
    { "builtin?",   "x",    bi_builtinp },
    { "special?",   "x",    bi_specialp },

    { "symbol",     "?l",    bi_symbol },
    { "=",          "'sx",  bi_setq },
    { "value",      "s",    bi_symbol_value },

    { "cons",       "xx",   bi_cons },
    { "car",        "l",    bi_car },
    { "cdr",        "l",    bi_cdr },
    { "setcar",     "cx",   bi_setcar },
    { "setcdr",     "cx",   bi_setcdr },

    { "==",         "nn",   bi_number_equal },
    { ">",          "nn",   bi_gt },
    { "<",          "nn",   bi_lt },
    { ">=",         "nn",   bi_gte },
    { "<=",         "nn",   bi_lte },

    { "+",          "nn",   bi_add },
    { "-",          "nn",   bi_sub },
    { "*",          "nn",   bi_mul },
    { "/",          "nn",   bi_div },
    { "%",          "nn",   bi_mod },
    { "++",         "n",    bi_inc },
    { "--",         "n",    bi_dec },

    { "bit-and",    "nn",   bi_bit_and },
    { "bit-or",     "nn",   bi_bit_or },
    { "bit-xor",    "nn",   bi_bit_xor },
    { "bit-neg",    "n",    bi_bit_neg },
    { "<<",         "nn",   bi_shift_left },
    { ">>",         "nn",   bi_shift_right },

    { "rawptr",     "x",    bi_rawptr },
    { "peek",       "n",    bi_peek },
    { "poke",       "nn",   bi_poke },
    { "sys",        "n",    bi_sys },

    { "read",       "",     read },
    { "print",      "x",    bi_print },
    { "open",       "ns",   bi_open },
    { "err",        "",     bi_err },
    { "eof",        "",     bi_eof },
    { "in",         "",     bi_in },
    { "out",        "x",    bi_out },
    { "terpri",     "",     bi_terpri },
    { "fresh-line", "",     bi_fresh_line },
    { "setin",      "n",    bi_setin },
    { "setout",     "n",    bi_setout },
    { "putback",    "",     bi_putback },
    { "close",      "n",    bi_close },
    { "load",       "s",    bi_load },

    { "fn",         "'s'+", bi_define },
    { "var",        "'sx",  bi_define },
    { "special",    "'s'+", bi_special },
    { "undef",      "s",    bi_undef },
    { "universe",   "",     bi_universe },
    { "gc",         "",     bi_gc },
#ifndef NAIVE
    { "error",      "?x",   bi_error },
    { "ignore",    "",      bi_ignore },
    { "stack",      "",     bi_stack },
#endif
    { "quit",       "x",    bi_quit },
    { "exit",       "?n",   bi_exit },

    { "butlast",    "l",    bi_butlast },
    { "copy-list",  "l",    bi_copy_list },
    { "last",       "l",    bi_last },
    { "length",     "l",    bi_length },
    { "member",     "xl",   bi_member },
    { "remove",     "xl",   bi_remove },
    { "@",          "fl",   bi_filter },

#ifndef NDEBUG
    { "debug",      "",     bi_debug },
#endif
#ifndef NO_DEBUGGER
    { "debugger",   "",     bi_debugger },
#endif

    { NULL, NULL }
};

void
init_quoting (void)
{
    // Make symbols for quoting.
    quote           = make_symbol ("quote", 5);
    expand_universe (quote);
    quasiquote      = make_symbol ("quasiquote", 10);
    expand_universe (quasiquote);
    unquote         = make_symbol ("unquote", 7);
    expand_universe (unquote);
    unquote_spliced = make_symbol ("unquote-spliced", 15);
    expand_universe (unquote_spliced);
}

void
init_io_symbols (void)
{
    lispptr i;
    lispptr o;

    // Make symbols to hold current I/O channel numbers.
    fnin  = STDIN;
    fnout = STDOUT;
    i = make_number (STDIN);
    o = make_number (STDOUT);

    tmp  = make_symbol ("stdin", 5);
    SET_SYMBOL_VALUE(tmp, i);
    expand_universe (tmp);

    tmp = make_symbol ("stdout", 6);
    SET_SYMBOL_VALUE(tmp, o);
    expand_universe (tmp);

    lisp_fnin  = make_symbol ("fnin", 4);
    SET_SYMBOL_VALUE(lisp_fnin, i);
    expand_universe (lisp_fnin);

    lisp_fnout = make_symbol ("fnout", 5);
    SET_SYMBOL_VALUE(lisp_fnout, o);
    expand_universe (lisp_fnout);
}

extern void test (void);

int
main (int argc, char * argv[])
{
    (void) argc, (void) argv;

    simpleio_init ();
    if (!init_heap ()) {
        outs ("No memory.");
        exit (EXIT_FAILURE);
    }
#ifdef TARGET_UNIX
    test ();
#endif
    init_eval ();
    add_builtins (builtins);
    init_quoting ();
    init_io_symbols ();
#ifndef NO_ONERROR
    init_onerror ();
#endif
    init_repl ();

#ifdef GC_STRESS
    do_gc_stress = true;
#endif

    load ("env-0.lisp");
#ifndef NDEBUG
    load ("smoke-test.lisp");
#endif
    load ("env-1.lisp");
#ifndef NDEBUG
    load ("test.lisp");
#endif
    load ("env-2.lisp");
#ifndef NDEBUG
#ifndef NO_ONERROR
    load ("test-onerror.lisp");
#endif
#endif
#ifndef TARGET_C16
    load ("env-3.lisp");
#endif
    do_break_repl = do_continue_repl = false;
    num_repls = -1;
    lisp_repl (REPL_STD);

    setout (STDOUT);
    outs ("Bye!");
    return 0;
}
