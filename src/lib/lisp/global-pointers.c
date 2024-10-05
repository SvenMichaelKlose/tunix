#include <ctype.h>
#include <stddef.h>
#include <stdbool.h>
#include <setjmp.h>

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

extern lispptr needle;

lispptr * global_pointers[] = {
    &universe, &t,
    &delayed_eval,
    &block_sym,
    &quote,
#ifndef NO_QUASIQUOTE
    &quasiquote, &unquote, &unquote_spliced,
#endif
    &return_sym, &return_name, &return_value,
    &go_sym, &go_tag,
    &current_expr,
#ifndef NO_MACROEXPAND
    &unexpanded_toplevel,
#endif
#ifndef NAIVE
    &current_toplevel,
    &current_function,
    &unevaluated_arg1,
    &error_info,
#endif
#ifndef NO_ONERROR
    &onerror_sym,
    &fail_sym,
#endif
#ifndef NO_DEBUGGER
    &debug_step,
    &breakpoints_sym,
    &repl_value,
    &highlighted,
#endif
#ifndef NO_MACROEXPAND
    &macroexpand_sym,
#endif
#if !defined(NO_VERBOSE_LOAD) && !defined(NO_VERBOSE_DEFINES)
    &vp_symbol,
#endif

    &lisp_fnin, &lisp_fnout,

    &x, &args, &argdefs, &arg1, &arg2, &arg2c,
    &list_start, &list_last,
    &value,
    &needle,

    &tmp, &tmp2,
    &make_cons_tmp,
    &make_cons_car,
    &make_cons_cdr,
    NULL
};
