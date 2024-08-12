#include <ctype.h>
#include <stddef.h>
#include <stdbool.h>
#include <setjmp.h>

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

lispptr * global_pointers[] = {
    &universe, &t,
    &delayed_eval,
    &block_sym,
    &quote, &quasiquote, &unquote, &unquote_spliced,
    &return_sym, &return_name, &return_value,
    &go_sym, &go_tag,
    &current_expr,
#ifndef NAIVE
    &current_toplevel,
#endif
    &current_function,
    &unexpanded_toplevel,
    &unevaluated_arg1,
#ifndef NO_DEBUGGER
    &onerror_sym,
    &debug_step,
    &breakpoints_sym,
    &debugger_return_value_sym,
#endif
#ifndef NO_MACROEXPAND
    &macroexpand_sym,
#endif
    &highlighted,

    &lisp_fnin, &lisp_fnout,

    // To be safe:
    &x, &args, &argdefs, &arg1, &arg2, &arg2c,
    &list_start, &list_last,
    &value, &va,
    NULL
};
