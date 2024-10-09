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

#ifdef USE_ZEROPAGE
#pragma bss-name (push, "ZEROPAGE")
#endif
extern lispptr x;
extern lispptr args;
extern lispptr arg1;
extern lispptr arg2c;
extern lispptr arg2;
extern lispptr value;
extern lispptr tmp;
#ifdef USE_ZEROPAGE
#pragma zpsym ("x")
#pragma zpsym ("args")
#pragma zpsym ("arg1")
#pragma zpsym ("arg2c")
#pragma zpsym ("arg2")
#pragma zpsym ("value")
#pragma zpsym ("tmp")
#pragma bss-name (pop)
#endif // #ifdef USE_ZEROPAGE

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

void FASTCALL bi_out_list (lispptr);

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
bi_set (void)
{
    SET_SYMBOL_VALUE(arg1, arg2);
    return arg2;
}

lispptr
bi_symbol_value (void)
{
    if (NOT(arg1))
        return arg1;
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
#ifndef NAIVE
    if (arg1)
        error_cons_expected (arg1);
#endif

    return s;
}

#ifndef NO_BUILTIN_CHAR_AT

lispptr
bi_char_at ()
{
    lispnum_t n = NUMBER_VALUE(arg2);
    lispobj_size_t l = SYMBOL_LENGTH(arg1);
    if (n < 0 || n >= l)
        return nil;
    return make_number (SYMBOL_NAME(arg1)[n]);
}

lispptr
bi_set_char_at ()
{
    lispptr        s = LIST_CAR(arg1);
    lispptr        n = LIST_CAR(LIST_CDR(arg1));
    lispptr        v = LIST_CAR(LIST_CDR(CDR(arg1)));
    lispnum_t      i = NUMBER_VALUE(n);
    lispobj_size_t l = SYMBOL_LENGTH(s);
    if (i < 0 || i >= l)
        return nil;
    SYMBOL_NAME(s)[i] = NUMBER_VALUE(v);
    return arg1;
}

#endif // #ifndef NO_BUILTIN_CHAR_AT

#ifndef NO_BUILTIN_GROUP_SYMBOL_NAME

lispptr
bi_symbol_name ()
{
    lispobj_size_t i = 0;
    lispobj_size_t l;

    if (NOT(arg1))
        return arg1;

    l = SYMBOL_LENGTH(arg1);
    list_start = list_last = nil;
    for (i = 0; i < l; i++) {
        tmp = make_cons (make_number (SYMBOL_NAME(arg1)[i]), nil);
        if (NOT_NIL(list_last)) {
            SETCDR(list_last, tmp);
            list_last = tmp;
        } else
            list_start = list_last = tmp;
    }
    list_last = nil;
    return list_start;
}

lispptr
bi_slength (void)
{
    return make_number (SYMBOL_LENGTH(arg1));
}

#endif // #ifndef NO_BUILTIN_GROUP_SYMBOL_NAME

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

#ifndef NO_BUILTIN_NTHCDR

lispptr
bi_nthcdr (void)
{
    int n = NUMBER_VALUE(arg1);
#ifndef NAIVE
    if (n < 0) {
        error_info = arg1;
        error (ERROR_NEGATIVE, "< 0");
        return nil;
    }
#endif
    while (n-- && CONSP(arg2))
        arg2 = CDR(arg2);
#ifndef NAIVE
    if (ATOM(arg2) && NOT_NIL(arg2))
        return error_cons_expected (arg2);
#endif
    return arg2;
}

#endif // #ifndef NO_BUILTIN_NTHCDR

#ifndef NO_BUILTIN_ASSOC

lispptr
bi_assoc ()
{
    DOLIST(x, arg2) {
        tmp = CAR(x);
        if (!CONSP(tmp))
            break;
        tmp = CAR(tmp);
        if (tmp == arg1)
            return CAR(x);
        if (NUMBERP(tmp) && NUMBERP(arg1))
            if (NUMBER_VALUE(tmp) == NUMBER_VALUE(arg1))
                return CAR(x);
    }
    return nil;
}

#endif // #ifndef NO_BUILTIN_ASSOC

#ifndef NO_BUILTIN_APPEND

lispptr
bi_append ()
{
    if (!unevaluated)
        x = eval_list ();
    list_start = list_last = nil;
    while (CONSP(x)) {
        // Skip NILs in arguments.
        while (CONSP(x) && !CONSP(CAR(x))) {
#ifndef NAIVE
            if (NOT_NIL(CAR(x)))
                return error_cons_expected (x);
#endif
            x = CDR(x);
        }

        // Break on end of argument list.
        if (NOT(x))
            goto done;

        // Copy first element.
        arg1 = CAR(x);
        tmp = make_cons (CAR(arg1), nil);
        if (NOT_NIL(list_last))
            SETCDR(list_last, tmp);
        list_last = tmp;

        if (NOT(list_start))
            list_start = list_last;

        // Append rest of elements.
        DOLIST(arg1, CDR(arg1)) {
            // Copy element.
            tmp = make_cons (CAR(arg1), nil);

            // Append to last.
            SETCDR(list_last, tmp);
            list_last = tmp;
        }

#ifndef NAIVE
        if (NOT_NIL(arg1))
            return error_cons_expected (x);
#endif
        x = CDR(x);
    }

done:
    return list_start;
}

#endif // #ifndef NO_BUILTIN_APPEND

#ifndef NO_BUILTIN_NCONC

lispptr
bi_nconc (void)
{
    // Taking any arguments, we need to eval manually.
    args = unevaluated ? x : eval_list ();

    // Get first list and its tail.
    list_start = nil;
    while (NOT_NIL(args) && NOT(list_start)) {
        list_start = CAR(args);
        args = CDR(args);
    }
    if (NOT(list_start))
        return nil;
#ifndef NAIVE
    if (ATOM(list_start))
        return error_cons_expected (list_start);
#endif
    list_last = last (list_start);

    // Connect tails.
    while (NOT_NIL(args)) {
        tmp = CAR(args);
        if (NOT_NIL(tmp)) {
#ifndef NAIVE
            if (ATOM(tmp))
                return error_cons_expected (list_start);
#endif
            SETCDR(list_last, tmp);
            list_last = last (tmp);
        }
        args = CDR(args);
    }

    return list_start;
}

#endif // #ifndef NO_BUILTIN_NCONC

#ifndef NO_BUILTIN_SUBSEQ

lispptr
bi_subseq (void)
{
    int n;
    int nstart;
    int nend;
    lispptr start;
    lispptr end;
    bool has_end = false;

    // Evaluate arguments and dings them.
    args = eval_list ();
    // TODO: type-check list
    list_start = LIST_CAR(args);
    arg2c = LIST_CDR(args);
    start = LIST_CAR(arg2c);
    end = LIST_CAR(LIST_CDR(arg2c));

    // TODO: type-check start
    nstart = NUMBER_VALUE(start);
#ifndef NAIVE
    if (nstart < 0) {
        error_info = start;
        error (ERROR_NEGATIVE, "< 0");
        return nil;
    }
#endif

    // Get start of list.
    arg1 = start;
    arg2 = list_start;
    arg2 = bi_nthcdr ();

    // TODO: type-check end
    if (NOT_NIL(end)) {
        nend = NUMBER_VALUE(end);
#ifndef NAIVE
        if (nend < 0) {
            error_info = end;
            error (ERROR_NEGATIVE, "< 0");
            return nil;
        }
#endif
        n = nend - nstart;
        has_end = true;
    }

    // Copy until end.
    list_start = list_last = nil;
    while ((!has_end || n-- > 0) && CONSP(arg2)) {
        tmp = make_cons (CAR(arg2), nil);
        if (NOT(list_start))
            list_start = tmp;
        else
            SETCDR(list_last, tmp);
        list_last = tmp;
        arg2 = CDR(arg2);
    }

#ifndef NAIVE
    if (ATOM(arg2) && NOT_NIL(arg2))
        return error_cons_expected (arg2);
#endif

    return list_start;
}

#endif // #ifndef NO_BUILTIN_SUBSEQ

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

#ifndef NO_BUILTIN_GROUP_BITOPS

DEFOP(bi_bit_and, &);
DEFOP(bi_bit_or, |);
DEFOP(bi_bit_xor, ^);
DEFOP(bi_shift_left, <<);
DEFOP(bi_shift_right, >>);

lispptr
bi_bit_neg (void)
{
    return make_number (~NUMBER_VALUE(arg1));
}

#endif // #ifndef NO_BUILTIN_GROUP_BITOPS

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
#ifndef NAIVE
    PUSH(current_toplevel);
    current_toplevel = x;
    PUSH(current_function);
    current_function = nil;
#endif
    PUSH_TAG(TAG_DONE); // Tell to return from eval0().
    x = eval0 ();
#ifndef NAIVE
    POP(current_function);
    POP(current_toplevel);
#endif
    return x;
}

// Consing. Could be optimized away if moved to eval().
lispptr
bi_apply (void)
{
    args = copy_list (arg2, COPY_BUTLAST, nil);
    tmp  = LIST_CAR(last (arg2));

    if (NOT_NIL(args))
        SETCDR(last (args), tmp);
    else
        args = tmp;

#ifndef NAIVE
    if (!LISTP(tmp)) {
        error (ERROR_TYPE, "Last arg isn't list");
        return nil;
    }
#endif

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
    while (NOT_NIL(x)) {
        // Get condition.
        arg1 = CAR(x);

        // It's the consequence with nothing following.
        arg2c = CDR(x);
        if (NOT(arg2c)) {
            HIGHLIGHT(x);
            x = arg1;
            return delayed_eval;
        }

        // Evaluate condition.
        HIGHLIGHT(x);
        PUSH(arg2c);
        x = arg1;
        tmp = eval ();
        POP(arg2c);

#ifndef NAIVE
        if (error_code)
            break;
#endif

        // Do consequence if condition isn't NIL.
        if (NOT_NIL(tmp)) {
            x = CAR(arg2c);
            HIGHLIGHT(arg2c);
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
        HIGHLIGHT(x);
        x = CAR(x);
        value = eval ();
        POP(x);
#ifndef NAIVE
        if (error_code)
            return nil;
#endif
        if (NOT(value))
            return nil;
    }
#ifndef NAIVE
    if (NOT_NIL(x))
        error_cons_expected (x);
#endif
    return value;
}

lispptr
bi_or (void)
{
    DOLIST(x, x) {
        PUSH(x);
        HIGHLIGHT(x);
        x = CAR(x);
        value = eval ();
        POP(x);
#ifndef NAIVE
        if (error_code)
            break;
#endif
        if (NOT_NIL(value))
            return value;
    }
#ifndef NAIVE
    if (NOT_NIL(x))
        error_cons_expected (x);
#endif
    return nil;
}

#ifndef NO_BUILTIN_PRINT

lispptr
bi_print (void)
{
    return print (arg1);
}

#endif // #ifndef NO_BUILTIN_PRINT

#ifndef NO_BUILTIN_GROUP_FILE

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

lispptr FASTCALL
nil4zero (char c)
{
    return c ? make_number (c) : nil;
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
    return nil4zero (c);
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
bi_conin (void)
{
    return nil4zero (conin ());
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

int countdown;
char * ostr;

void
bi_out_flush (void)
{
    if (ostr != buffer) {
        outsn (buffer, ostr - buffer);
        ostr = buffer;
    }
}

#ifdef __CC65__
inline
#endif
void FASTCALL
bi_out_c (char c)
{
    *ostr++ = c;
    if (ostr == &buffer[MAX_SYMBOL])
        bi_out_flush ();
}

void
counted_out (char c)
{
    if (countdown < 0) {
        bi_out_c (c);
    } else if (countdown) {
        bi_out_c (c);
        countdown--;
    }
}

void FASTCALL
bi_out_atom (lispptr x)
{
    lispobj_size_t l;

    if (NUMBERP(x)) {
        counted_out (NUMBER_VALUE(x));
    } else if (_NAMEDP(x)) {
        tmpstr = SYMBOL_NAME(x);
        for (l = SYMBOL_LENGTH(x); l; --l)
            counted_out (*tmpstr++);
    } else {
        bi_out_flush ();
        print (x);
    }
}

void FASTCALL
bi_out_list (lispptr x)
{
    DOLIST(tmp, x) {
        PUSH(tmp);
        bi_out_atom (CAR(tmp));
        POP(tmp);
    }
}

lispptr
bi_out (void)
{
    ostr = buffer;
    bi_out_list (arg1);
    countdown = -1;
    bi_out_flush ();
    return arg1;
}

lispptr
bi_outlim (void)
{
    countdown = NUMBER_VALUE(arg1);
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

#ifndef NO_BUILTIN_READLINE

lispptr
bi_read_line (void)
{
    lispobj_size_t l;

    tmpstr = buffer;
    if (fnin != STDIN) {
        while (!eof ()) {
            tmpc = in ();
            if (tmpc != 13)
                break;
        }
    }
    putback ();
    for (l = 0; l < MAX_SYMBOL - 1; l++) {
        if (eof ())
            break;
        tmpc = in ();
        if ((fnin == STDIN && tmpc == 13) || tmpc == 10)
            break;
        *tmpstr++ = tmpc;
    }
    return make_symbol (buffer, tmpstr - buffer);
}

#endif // #ifndef NO_BUILTIN_READLINE

#endif // #ifndef NO_BUILTIN_GROUP_FILE

#if !defined(NO_BUILTIN_GROUP_DIRECTORY) && defined(__CC65__)

#include <cbm.h>

lispptr
bi_opendir (void)
{
    simpleio_chn_t chn = directory_open ();
    return nil4zero (chn);
}

struct cbm_dirent dirent;

lispptr
bi_readdir (void)
{
    lispobj_size_t l;
    char i;
    char err;

    err = directory_read ((simpleio_chn_t) NUMBER_VALUE(arg1), &dirent);
    if (err)
        return nil;
    memcpy (buffer, dirent.name, sizeof (dirent.name));
    buffer[sizeof (dirent.name)] = 0;
    l = strlen (buffer);
    for (i = 0; i < l; i++)
        buffer[i] = reverse_case (buffer[i]);
    list_start = make_cons (make_symbol (buffer, l), nil);
    tmp = make_cons (make_number (dirent.size), nil);
    SETCDR(list_start, tmp);
    PUSH(tmp);
    tmp2 = make_cons (make_number (dirent.type), nil);
    POP(tmp);
    SETCDR(tmp, tmp2);
    return list_start;
}

lispptr
bi_closedir (void)
{
    directory_close ((simpleio_chn_t) NUMBER_VALUE(arg1));
    return nil;
}

#endif // #if !defined(NO_BUILTIN_GROUP_DIRECTORY) && defined(__CC65__)

#ifndef NO_BUILTIN_LOAD

lispptr
bi_load (void)
{
    name_to_buffer (arg1);
    if (!load (buffer))
        return nil;

#ifdef GC_AFTER_LOAD_THRESHOLD
    if (heap_free_size () < GC_AFTER_LOAD_THRESHOLD)
        gc ();
#endif

    return t;
}

#endif // #ifndef NO_BUILTIN_LOAD

#ifndef NO_BUILTIN_GROUP_IMAGE

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
    return BOOL(image_save (buffer));
}

#endif // #ifndef NO_BUILTIN_GROUP_IMAGE

#ifndef NO_BUILTIN_GROUP_DEFINITIONS

lispptr
bi_define (void)
{
#ifndef NO_VERBOSE_DEFINES
    simpleio_chn_t old_out = fnout;

    if (NOT_NIL(SYMBOL_VALUE(vp_symbol))) {
        setout (STDOUT);
        if (NOT_NIL(member (arg1, SYMBOL_VALUE(universe))))
            outs ("Redefining ");
        else
            outs ("Defining ");
        print (arg1);
        terpri ();
        setout (old_out);
    }
#endif
    if (NOT(member (arg1, SYMBOL_VALUE(universe))))
        expand_universe (arg1);
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

#endif // #ifndef NO_BUILTIN_GROUP_DEFINITIONS

#ifndef NO_BUILTIN_GC

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

#endif // #ifndef NO_BUILTIN_GC

#ifndef NO_BUILTIN_FREE

lispptr
bi_free (void)
{
    return make_number (heap_free_size ());
}

#endif // #ifndef NO_BUILTIN_FREE

#ifndef NAIVE

lispptr
bi_error (void)
{
    last_errstr = "User error";
    if (NOT_NIL(arg1))
        current_expr = arg1;
    error_code = ERROR_USER;
#ifndef NO_BUILTIN_GROUP_FILE
    setout (STDOUT);
    bi_out_list (make_cons (make_symbol ("ERROR: ", 7), arg1));
#endif
    return nil;
}

lispptr
bi_ignore (void)
{
    do_break_repl = BRK_CONTINUE;
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
        fresh_line ();
        outn (i++);
        outs (": ");
        print (*p);
    }
    setout (old_out);
    return nil;
}
#endif // #ifndef NAIVE

lispptr
bi_exit (void)
{
    if (NOT_NIL(arg1))
        exit (NUMBER_VALUE(arg1));
    else
        do_break_repl = BRK_EXIT;
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
    DOLIST(tmp, arg2) {
        tmp2 = CAR(tmp);
        if (tmp2 == arg1 ||
            (NUMBERP(tmp2) && NUMBERP(arg1) &&
             NUMBER_VALUE(tmp2) == NUMBER_VALUE(arg1)))
            return tmp;
    }
#ifndef NAIVE
    if (NOT_NIL(tmp))
        error_cons_expected (tmp);
#endif
    return nil;
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
    HIGHLIGHT(x);
    list_start = list_last = make_cons (eval0 (), nil);
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
        HIGHLIGHT(x);
        tmp = make_cons (eval0 (), nil);
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
        HIGHLIGHT(x);
        tmp = eval0 ();
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

#if !defined(NO_BUILTIN_TIME) && defined(__CC65__)
char bekloppies[sizeof (long)];

lispptr
bi_time (void)
{
    asm ("cli");
#if defined(TARGET_C128) || defined(TARGET_C64) || defined(TARGET_VIC20)
    bekloppies[0] = *(char *) 0xa2;
    bekloppies[1] = *(char *) 0xa1;
    bekloppies[2] = *(char *) 0xa0;
#elif defined(TARGET_C16) || defined(TARGET_PLUS4)
    bekloppies[0] = *(char *) 0xa5;
    bekloppies[1] = *(char *) 0xa4;
    bekloppies[2] = *(char *) 0xa3;
#elif defined (TARGET_PET)
    bekloppies[0] = *(char *) 0x8f;
    bekloppies[1] = *(char *) 0x8e;
    bekloppies[2] = *(char *) 0x8d;
#endif
    bekloppies[3] = 0;
    asm ("sei");
    return make_number (*(long *) bekloppies);
}
#endif // #if !defined(NO_BUILTIN_TIME) && defined(__CC65__)

#ifdef TARGET_UNIX

long bekloppies_start;

long
bekloppies (void)
{
    struct timespec ts;
    if (clock_gettime (CLOCK_REALTIME, &ts) == 0)
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

#ifdef NO_BUILTIN_TIME

lispptr
bi_time (void)
{
    return make_number (0);
}

#endif // #ifdef NO_BUILTIN_TIME

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

    // BLOCK is hard-wired into "eval.c".
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
    { "set",          "sx",   bi_set },
    { "=",            "'sx",  bi_set },
    { "symbol-value", "s",    bi_symbol_value },

#ifndef NO_BUILTIN_GROUP_SYMBOL_NAME
    { "symbol-name",  "S",    bi_symbol_name },
    { "slength",      "S",    bi_slength },
#endif
#ifndef NO_BUILTIN_CHAR_AT
    { "char-at",     "sn",  bi_char_at },
    { "set-char-at", "+x",  bi_set_char_at },
#endif

    { "cons",       "xx",   bi_cons },
    { "car",        "l",    bi_car },
    { "cdr",        "l",    bi_cdr },
    { "setcar",     "cx",   bi_setcar },
    { "setcdr",     "cx",   bi_setcdr },

#ifndef NO_BUILTIN_NTHCDR
    { "nthcdr",     "nx",   bi_nthcdr },
#endif
#ifndef NO_BUILTIN_ASSOC
    { "assoc",      "xl",   bi_assoc },
#endif // #ifndef NO_BUILTIN_ASSOC
#ifndef NO_BUILTIN_APPEND
    { "append",     NULL,   bi_append },
#endif
#ifndef NO_BUILTIN_NCONC
    { "nconc",      NULL,   bi_nconc },
#endif
#ifndef NO_BUILTIN_SUBSEQ
    { "subseq",     NULL,   bi_subseq },
#endif

#ifndef NO_BUILTIN_GROUP_ARITH
    { "==", "nn",   bi_number_equal },
    { ">",  "nn",   bi_gt },
    { "<",  "nn",   bi_lt },
    { ">=", "nn",   bi_gte },
    { "<=", "nn",   bi_lte },

    { "+",  "nn",   bi_add },
    { "-",  "nn",   bi_sub },
    { "*",  "nn",   bi_mul },
    { "/",  "nn",   bi_div },
    { "%",  "nn",   bi_mod },
    { "++", "n",    bi_inc },
    { "--", "n",    bi_dec },
#endif

#ifndef NO_BUILTIN_GROUP_BITOPS
    { "bit-and",    "nn",   bi_bit_and },
    { "bit-or",     "nn",   bi_bit_or },
    { "bit-xor",    "nn",   bi_bit_xor },
    { "bit-neg",    "n",    bi_bit_neg },
    { "<<",         "nn",   bi_shift_left },
    { ">>",         "nn",   bi_shift_right },
#endif

#ifndef NO_BUILTIN_GROUP_RAW_ACCESS
    { "rawptr",     "x",    bi_rawptr },
    { "peek",       "n",    bi_peek },
    { "poke",       "nn",   bi_poke },
#ifndef TARGET_CPM
    { "sys",        "n",    bi_sys },
#endif
#endif // #ifndef NO_BUILTIN_GROUP_RAW_ACCESS

#ifndef NO_BUILTIN_READ
    { "read",       "",     read_expr },
#endif
#ifndef NO_BUILTIN_PRINT
    { "print",      "x",    bi_print },
#endif

#ifndef NO_BUILTIN_GROUP_FILE
#ifndef NO_BUILTIN_LOAD
    { "load",       "s",    bi_load },
#endif
    { "open",       "ss",   bi_open },
    { "err",        "",     bi_err },
    { "eof",        "",     bi_eof },
    { "conin",      "",     bi_conin },
    { "in",         "",     bi_in },
#ifndef NO_BUILTIN_READ_LINE
    { "read-line",  "",     bi_read_line },
#endif
    { "out",        "+x",   bi_out },
    { "outlim",     "n",    bi_outlim },
    { "terpri",     "",     bi_terpri },
    { "fresh-line", "",     bi_fresh_line },
    { "setin",      "n",    bi_setin },
    { "setout",     "n",    bi_setout },
    { "putback",    "",     bi_putback },
    { "close",      "n",    bi_close },
#endif

#if !defined(NO_BUILTIN_GROUP_DIRECTORY) && defined(__CC65__)
    { "opendir",    "",     bi_opendir },
    { "readdir",    "n",    bi_readdir },
    { "closedir",   "n",    bi_closedir },
#endif

#ifndef NO_BUILTIN_GROUP_IMAGE
    { "iload",      "s",    bi_iload },
    { "isave",      "s",    bi_isave },
#endif

#ifndef NO_BUILTIN_GROUP_DEFINITIONS
    { "fn",         "'s'+", bi_define },
    { "var",        "'sx",  bi_define },
    { "special",    "'s'+", bi_special },
#endif

#ifndef NO_BUILTIN_GC
    { "gc",         "",     bi_gc },
#endif
#ifndef NO_BUILTIN_FREE
    { "free",       "",     bi_free },
#endif

#ifndef NAIVE
    { "error",      "+x",   bi_error },
    { "ignore",    "",      bi_ignore },
    { "stack",      "",     bi_stack },
#endif
    { "exit",       "?n",   bi_exit },

    { "butlast",    "l",    bi_butlast },
    { "copy-list",  "l",    bi_copy_list },
    { "last",       "l",    bi_last },
    { "length",     "x",    bi_length },
    { "member",     "xl",   bi_member },
    { "remove",     "xl",   bi_remove },
    { "@",          "fl",   bi_filter },

#ifndef TARGET_BUILTIN_TIME
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
#ifndef NO_BUILTIN_GROUP_FILE
    countdown = -1;
#endif
}
