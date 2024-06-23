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

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern lispptr x;
extern lispptr value;
lispptr arg1;
lispptr arg2c;
lispptr arg2;
extern lispptr tmp;
int len;
#ifdef __CC65__
#pragma zpsym ("x")
#pragma zpsym ("value")
#pragma zpsym ("arg1")
#pragma zpsym ("arg2c")
#pragma zpsym ("arg2")
#pragma zpsym ("tmp")
#pragma zpsym ("len")
#pragma bss-name (pop)
#endif

// libsimpleio channels.
char load_fn = 12;
lispptr lisp_fnin;
lispptr lisp_fnout;

// For READ to make expressions of abbreviations.
lispptr quote;
lispptr quasiquote;
lispptr unquote;
lispptr unquote_spliced;

lispptr go_expr;
lispptr return_expr;
lispptr return_args;

lispptr onerror;

// Building lists in loops.
lispptr start;  // First cons.
lispptr lastc;  // Last cons (to append to).

lispptr last_repl_expr;  // Read expression in REPL.
lispptr last_eval_expr;  // Expression to eval0().
char *  last_errstr;     // Addiitonal.
char    num_repls;       // Number of REPLs - 1.
bool    debug_mode;      // Unused.  Set by DEBUG.
bool    do_break_repl;   // Tells current REPL to return.
bool    do_continue_repl; // If do_break_repl, tell REPL to continue.
bool    do_exit_program; // Return to top-level REPL.

void FASTCALL
name_to_buffer (lispptr s)
{
    uchar len;
    len = SYMBOL_LENGTH(s);
    memcpy (buffer, SYMBOL_NAME(s), len);
    buffer[len] = 0;
}

int FASTCALL
length (lispptr x)
{
    len = 0;
    DOLIST(x, x)
        len++;
    return len;
}

lispptr FASTCALL
copy_list (lispptr x, bool do_butlast, lispptr find)
{
    if (ATOM(x))
        return x;
    if (do_butlast && !CDR(x))
        return nil;
    while (find && find == CAR(x)) {
        x = CDR(x);
        if (ATOM(x))
            return x;
    }
    PUSH(x);
    start = lastc = make_cons (CAR(x), nil);
    POP(x);
    PUSH(start);
    DOLIST(x, CDR(x)) {
        if (CONSP(x)) {
            if (do_butlast && !CDR(x))
                break;
            if (!find || find != CAR(x)) {
                PUSH(lastc);
                PUSH(x);
                tmp = make_cons (CAR(x), nil);
                POP(x);
                POP(lastc);
            }
        } else
            tmp = x;
        SETCDR(lastc, tmp);
        lastc = tmp;
    }
    POP(start);
    return start;
}

lispptr FASTCALL
butlast (lispptr x)
{
    return copy_list (x, true, nil);
}

lispptr FASTCALL
last (lispptr x)
{
    DOLIST(tmp2, x)
        if (ATOM(CDR(tmp2)))
            return tmp2;
    return nil;
}

lispptr FASTCALL
member (lispptr needle, lispptr haystack)
{
    DOLIST(x, haystack)
        if (CAR(x) == needle)
            return x;
    return nil;
}

void FASTCALL
make_call (lispptr args)
{
    x = make_cons (arg1, args);
    unevaluated = true;
    PUSH_TAG(TAG_DONE); // Tell to return from eval0().
}

void FASTCALL
make_car_call (void)
{
    make_call (make_cons (CAR(arg2), nil));
}

lispptr
bi_eq (void)
{
    return BOOL(arg1 == arg2); // TOOD: Return arg1 if T.
}

lispptr
bi_not (void)
{
    return BOOL(!arg1);
}

lispptr
bi_atom (void)
{
    if (!arg1)
        return t;
    return ATOM(arg1) ? arg1 : nil;
}

lispptr
bi_symbolp (void)
{
    if (!arg1)
        return t;
    return SYMBOLP(arg1) ? arg1 : nil;
}

lispptr
bi_builtinp (void)
{
    if (!arg1)
        return nil;
    return BUILTINP(arg1) ? arg1 : nil;
}

lispptr
bi_specialp (void)
{
    if (!arg1)
        return nil;
    return SPECIALP(arg1) ? arg1 : nil;
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

    // Allocate empty symbol of wanted length.
    len = length (arg1);
    PUSH(arg1);
    s = alloc_symbol (buffer, len);
    POP(arg1);

    // Make symbol name from list.
    p = SYMBOL_NAME(s);
    DOLIST(arg1, arg1) {
        if (!NUMBERP(CAR(arg1))) {
            error (ERROR_TYPE, "(string nlst)");
            break;
        }
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
    return make_number ((long) arg1);
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
    args = copy_list (arg2, true, nil);
    POP(arg2);
    tmp = CAR(last (arg2));
    if (args) {
        if (!LISTP(tmp)) {
            error (ERROR_TYPE, "Last arg isn't list!");
            POP(arg1);
            return nil;
        }
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

        PUSH(arg2c);
        x = arg1;
        tmp = eval ();
        POP(arg2c);
        if (has_error)
            break;
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
        x = CAR(x);
        value = eval ();
        POP(x);
        if (!value || has_error)
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
        if (has_error)
            break;
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

lispptr
bi_err (void)
{
    if (!err ())
        return nil;
    return make_number (err ());
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

void FASTCALL
load (char * pathname)
{
    int oldin = fnin;

    simpleio_open (load_fn, pathname, 'r');
    arg1 = make_number (load_fn);
    bi_setin ();
    if (err ()) {
        error (ERROR_FILE, pathname);
        goto err_open;
    }

    load_fn++;
    in (); putback ();
    while (!eof ()) {
        if (err ()) {
            error (ERROR_FILE, pathname);
            goto err_open;
        }
        last_repl_expr = x = read ();
        if (do_break_repl) {
            if (do_continue_repl) {
                do_break_repl = do_continue_repl = false;
                continue;
            }
            break;
        }
#ifdef VERBOSE_LOAD
        print (eval ());
        terpri ();
#else
        eval ();
#endif
    }
    load_fn--;

    simpleio_close (load_fn);
err_open:
    arg1 = make_number (oldin);
    bi_setin ();
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
    PTRTYPE(tmp) |= TYPE_SPECIAL;
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
    universe = copy_list (universe, false, arg1);
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
    return make_number (heap_end - heap_free);
}

lispptr
bi_error (void)
{
    last_errstr = "User error.";
    if (arg1)
        last_eval_expr = arg1;
    has_error = ERROR_USER;
    return nil;
}

lispptr
bi_noerror (void)
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
        out_number (i++);
        outs (": ");
        print (*p);
        terpri ();
    }
    setout (old_out);
    return nil;
}

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
    return copy_list (arg1, true, nil);
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
bi_copy_list (void)
{
    return copy_list (arg1, false, nil);
}

lispptr
bi_filter (void)
{
    PUSH(arg1);
    PUSH(arg2);
    make_car_call ();
    start = lastc = make_cons (eval0 (), nil);
    POP(arg2);
    POP(arg1);
    if (do_break_repl)
        return nil;

    PUSH(start);
    DOLIST(arg2, CDR(arg2)) {
        PUSH(arg1);
        PUSH(arg2);
        PUSH(lastc);
        make_car_call ();
        tmp = make_cons (eval0 (), nil);
        if (do_break_repl) {
            stack += 4 * sizeof (lispptr);
            return nil;
        }
        POP(lastc);
        SETCDR(lastc, tmp);
        lastc = tmp;
        POP(arg2);
        POP(arg1);
    }
    POP(start);
    return start;
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

    { "symbol",     "l",    bi_symbol },
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
    { "error",      "?x",   bi_error },
    { "noerror",    "",     bi_noerror },
    { "stack",      "",     bi_stack },
    { "quit",       "x",    bi_quit },
    { "exit",       "?n",   bi_exit },

    { "butlast",    "l",    bi_butlast },
    { "copy-list",  "l",    bi_copy_list },
    { "last",       "l",    bi_last },
    { "length",     "l",    bi_length },
    { "member",     "xl",   bi_member },
    { "@",          "fl",   bi_filter },

#ifndef NDEBUG
    { "debug",      "",     bi_debug },
#endif

    { NULL, NULL }
};

void
setstd (void)
{
    arg1 = make_number (STDIN);
    bi_setin ();
    arg1 = make_number (STDOUT);
    bi_setout ();
}

lispptr
lisp_repl ()
{
    // Save I/O channels.
    int old_in = fnin;
    int old_out = fnin;

    // Print waiting error message.
    if (has_error) {
#ifndef NO_ONERROR
        if (CONSP(SYMBOL_VALUE(onerror))) {
            x = make_cons (onerror, make_cons (make_number (has_error), make_cons (last_repl_expr, make_cons (last_eval_expr, nil))));
            has_error = false;
            unevaluated = true;
            PUSH_TAG(TAG_DONE);
            return eval0 ();
        }
#endif

        // Print error info.
        setout (STDERR);
        terpri ();
        outs ("In REPL: "); print (last_repl_expr); terpri ();
        outs ("Eval: ");    print (last_eval_expr); terpri ();
        outs ("Error #");   out_number (has_error); outs (": ");
        if (last_errstr)
            outs (last_errstr);
        terpri ();
        has_error = false;
    }

    num_repls++;

    // Read expresions from standard in until end of input
    // or QUIT has been invoked.
    setstd ();
    while (!eof ()) {
        // Print prompt with number of recursions.
        if (num_repls)
            out ('0' + num_repls);
        outs ("* ");

        // Read an expression.
        last_repl_expr = x = read ();
        fresh_line ();

        // Evaluate expression on program channels.
        arg1 = make_number (old_in);
        bi_setin ();
        arg1 = make_number (old_out);
        bi_setout ();
        x = eval ();
        if (has_error)
            x = lisp_repl ();

        // Break on demand.
        if (do_break_repl) {
            if (!do_exit_program) {
                do_break_repl = false;
                break;
            } else {
                if (do_continue_repl) {
                    do_continue_repl = false;
                    continue;
                }
                if (num_repls)
                    break;
            }
            setstd ();
            outs ("Program exited.");
            terpri ();
            do_break_repl = false;
            do_exit_program = false;
        }

        // Print result on standard out.
        setstd ();
        fresh_line ();
        print (x);
        fresh_line ();
    }

    // Restore former I/O channels.
    arg1 = make_number (old_in);
    bi_setin ();
    arg1 = make_number (old_out);
    bi_setout ();

    num_repls--;
    return x;
}

int
main (int argc, char * argv[])
{
    lispptr i;
    lispptr o;
    (void) argc, (void) argv;

    simpleio_init ();
    if (!lisp_init ()) {
        outs ("No memory.");
        exit (EXIT_FAILURE);
    }

    add_builtins (builtins);

    // Prepare quoting.
    quote           = make_symbol ("quote", 5);
    expand_universe (quote);
    quasiquote      = make_symbol ("quasiquote", 10);
    expand_universe (quasiquote);
    unquote         = make_symbol ("unquote", 7);
    expand_universe (unquote);
    unquote_spliced = make_symbol ("unquote-spliced", 15);
    expand_universe (unquote_spliced);

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

    onerror = make_symbol ("onerror", 7);
    expand_universe (onerror);

    load ("env.lisp");
    do_break_repl = do_continue_repl = false;
    num_repls = -1;
    lisp_repl ();

    setout (STDOUT);
    outs ("Bye!");
    return 0;
}
