#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#include <cbm.h>
#endif

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <setjmp.h>
#ifdef TARGET_UNIX
#include <signal.h>
#include <stdio.h>
#include <time.h>
#endif

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

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

// Set by DEBUG (bi_debug()).
// Used as breakpoint condition for host debugger.
#ifndef NDEBUG
bool debug_mode;
#endif

#ifdef __CC65__
#pragma code-name ("CODE_BUILTINS")
#endif

lispptr
bi_eq (void)
{
    return BOOL(arg1 == arg2);
}

lispptr
bi_not (void)
{
    return BOOL(NOT(arg1));
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
    int      len;
    lispptr  s;
    char *   p;

    // Get length.  Truncate at 255.
    len = length (arg1);
    if (len > 255)
        len = 255;

    // Allocate empty symbol of wanted length.
    s = alloc_symbol (buffer, len);

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
bi_char_at ()
{
    lispnum_t n = NUMBER_VALUE(arg2);
    lisp_len = SYMBOL_LENGTH(arg1);
    if (n < 0 || n >= lisp_len)
        return nil;
    return make_number (SYMBOL_NAME(arg1)[n]);
}

lispptr
bi_symbol_name ()
{
    lispobj_size_t i = 0;

    lisp_len = SYMBOL_LENGTH(arg1);
    list_last = nil;
    for (i = 0; i < lisp_len; i++) {
        tmp = make_cons (make_number (SYMBOL_NAME(arg1)[i]), nil);
        if (list_last) {
            SETCDR(list_last, tmp);
            list_last = tmp;
        } else
            list_start = list_last = tmp;
    }
    list_last = nil;
    return list_start;
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
    SETCAR(arg1, arg2);
    return arg1;
}

lispptr
bi_setcdr (void)
{
    SETCDR(arg1, arg2);
    return arg1;
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

#ifndef TARGET_CPM
lispptr
bi_sys (void)
{
    ((void (*) (void)) NUMBER_VALUE(arg1)) ();
    return nil;
}
#endif // #ifndef TARGET_CPM

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
    args = copy_list (arg2, COPY_BUTLAST, nil);
    tmp  = LIST_CAR(last (arg2));

    if (args) {
#ifndef NAIVE
        if (!LISTP(tmp)) {
            error (ERROR_TYPE, "Last arg isn't list");
            return nil;
        }
#endif
        SETCDR(last (args), tmp);
    } else
        args = tmp;

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
    return_name  = arg2;
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
        // Get condition.
        arg1 = CAR(x);

        // It's the consequence with nothing following.
        arg2c = CDR(x);
        if (NOT(arg2c)) {
#ifndef NO_DEBUGGER
            highlighted = x;
#endif
            x = arg1;
            return delayed_eval;
        }

        // Evaluate condition.
#ifndef NO_DEBUGGER
        PUSH_HIGHLIGHTED(x);
#endif
        PUSH(arg2c);
        x = arg1;
        tmp = eval ();
        POP(arg2c);
#ifndef NO_DEBUGGER
        POP_HIGHLIGHTED();
#endif

#ifndef NAIVE
        if (error_code)
            break;
#endif

        // Do consequence if condition isn't NIL.
        if (tmp) {
            x = CAR(arg2c);
#ifndef NO_DEBUGGER
            highlighted = arg2c;
#endif
            return delayed_eval;
        }

        // Step to next condition.
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
        if (NOT(value))
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
    char mode = SYMBOL_NAME(arg2)[0];
    simpleio_chn_t c;

#ifndef NAIVE
    if (SYMBOL_LENGTH(arg2) != 1 || (mode != 'r' && mode != 'w')) {
        error (ERROR_FILEMODE, "Ill file mode");
        return nil;
    }
#endif // #ifndef NAIVE
    name_to_buffer (arg1);
    c = simpleio_open (buffer, mode);
    if (c)
        return make_number (c);
    return nil;
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
    if (NOT(arg1))
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

lispptr bi_gc (void);

lispptr
bi_load (void)
{
#ifdef VERBOSE_LOAD
    outs ("Loading "); print (arg1); terpri ();
#endif
    name_to_buffer (arg1);
    load (buffer);
#ifdef GC_AFTER_LOAD_THRESHOLD
    if (heap_free_size () < GC_AFTER_LOAD_THRESHOLD)
        bi_gc ();
#endif
    return nil;
}

#ifndef NO_IMAGES

lispptr
bi_iload (void)
{
    name_to_buffer (arg1);
    if (image_load (buffer))
        longjmp (restart_point, 1);
    return nil;
}

lispptr
bi_isave (void)
{
    name_to_buffer (arg1);
    if (image_save (buffer))
        return t;
    return nil;
}

#endif // #ifndef NO_IMAGES

lispptr
bi_define (void)
{
#ifdef VERBOSE_DEFINES
    if (member (arg1, SYMBOL_VALUE(universe)))
        outs ("Redefining ");
    else {
        expand_universe (arg1);
        outs ("Defining ");
    }
    print (arg1);
    terpri ();
#else
    if (!member (arg1, SYMBOL_VALUE(universe)))
        expand_universe (arg1);
#endif
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
bi_gc (void)
{
#ifdef COMPRESSED_CONS
    do_compress_cons = true;
#endif
    gc ();
#ifdef COMPRESSED_CONS
    do_compress_cons = false;
#endif

    return make_number (heap_free_size ());
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
    if (NOT(arg2))
        return nil;
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
            stack += sizeof (lispptr) << 2;
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
            stack += sizeof (lispptr) << 1;
            return nil;
        }
        POP(list_last);
        SETCDR(list_last, tmp);
    }
    POP(list_start);
    return list_start;
}

#if defined(TARGET_C128) || defined(TARGET_C16) || defined(TARGET_C64) || defined(TARGET_PET) || defined(TARGET_PLUS4) || defined(TARGET_VIC20)
char bekloppies[sizeof (long)];

lispptr
bi_time (void)
{
    asm ("cli");
#if defined(TARGET_C16) || defined(TARGET_PLUS4)
    bekloppies[0] = *(char *) 0xa5;
    bekloppies[1] = *(char *) 0xa4;
    bekloppies[2] = *(char *) 0xa3;
#else
    bekloppies[0] = *(char *) 0xa2;
    bekloppies[1] = *(char *) 0xa1;
    bekloppies[2] = *(char *) 0xa0;
#endif
    bekloppies[3] = 0;
    asm ("sei");
    return make_number (*(long *) bekloppies);
}
#endif // #if defined(TARGET_C128) || defined(TARGET_C16) || defined(TARGET_C64) || defined(TARGET_PET) || defined(TARGET_PLUS4) || defined(TARGET_VIC20)

#ifdef TARGET_UNIX

long bekloppies_start;

long
bekloppies (void)
{
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) == 0)
        return ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
    perror("clock_gettime");
    return 0;
}

lispptr
bi_time (void)
{
    return make_number (bekloppies () - bekloppies_start);
}

#endif // #ifdef TARGET_UNIX

#ifndef NDEBUG
lispptr
bi_debug (void)
{
    debug_mode = true;
#ifdef TARGET_UNIX
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

const struct builtin builtins[] = {
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

    { "symbol",       "?l",   bi_symbol },
    { "=",            "'sx",  bi_setq },
    { "symbol-value", "s",    bi_symbol_value },
    { "symbol-name",  "s",    bi_symbol_name },
    { "char-at",      "sn",   bi_char_at },

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
#ifndef TARGET_CPM
    { "sys",        "n",    bi_sys },
#endif // #ifndef TARGET_CPM

    { "read",       "",     read },
    { "print",      "x",    bi_print },
    { "open",       "ss",   bi_open },
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

#ifndef NO_IMAGES
    { "iload",      "s",    bi_iload },
    { "isave",      "s",    bi_isave },
#endif

    { "fn",         "'s'+", bi_define },
    { "var",        "'sx",  bi_define },
    { "special",    "'s'+", bi_special },
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

#ifndef TARGET_CPM
    { "time",       "",     bi_time },
#endif

#ifndef NDEBUG
    { "debug",      "",     bi_debug },
#endif
#ifndef NO_DEBUGGER
    { "debugger",   "",     bi_debugger },
#endif

    { NULL, NULL }
};

#ifdef __CC65__
#pragma code-name ("CODE_INIT")
#endif

void
init_builtins (void)
{
    add_builtins (builtins);
}
