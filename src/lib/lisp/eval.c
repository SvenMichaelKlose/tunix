#include <ingle/cc65-charmap.h>

#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

char * stack_start;
lispptr current_expr;
lispptr current_toplevel;

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern char * msg;
lispptr x;
lispptr args;
lispptr arg1;
lispptr arg2c;
lispptr arg2;
lispptr tmp;
lispptr value;
char * stack;
char * stack_end;
char * tagstack_start;
char * tagstack_end;
char * tagstack;
lispptr name;
lispptr defs;
lispptr value;
struct builtin * bfun;
lispptr va;
lispptr delayed_eval;
lispptr block_sym;
lispptr return_sym;
lispptr return_name;
lispptr return_value;
lispptr go_sym;
lispptr go_tag;
lispptr debug_step;
bool do_invoke_debugger;
bool tag_found;
#ifndef NAIVE
char has_error;
#endif
uchar c;
char * badef;
uchar na;
bool unevaluated;
#ifdef __CC65__
#pragma zpsym ("tmp")
#pragma zpsym ("msg")
#pragma zpsym ("x")
#pragma zpsym ("args")
#pragma zpsym ("arg1")
#pragma zpsym ("arg2c")
#pragma zpsym ("arg2")
#pragma zpsym ("value")
#pragma zpsym ("stack")
#pragma zpsym ("stack_end")
#pragma zpsym ("tagstack_start")
#pragma zpsym ("tagstack_end")
#pragma zpsym ("tagstack")
#pragma zpsym ("name")
#pragma zpsym ("defs")
#pragma zpsym ("value")
#pragma zpsym ("bfun")
#pragma zpsym ("va")
#pragma zpsym ("delayed_eval")
#pragma zpsym ("block_sym")
#pragma zpsym ("return_sym")
#pragma zpsym ("return_name")
#pragma zpsym ("return_value")
#pragma zpsym ("go_sym")
#pragma zpsym ("go_tag")
#pragma zpsym ("tag_found")
#ifndef NAIVE
#pragma zpsym ("has_error")
#endif
#pragma zpsym ("c")
#pragma zpsym ("badef")
#pragma zpsym ("na")
#pragma zpsym ("unevaluated")
#pragma bss-name (pop)
#endif

// Evaluate list to list of return values.
// TODO: Inline into eval0.
lispptr
eval_list (void)
{
    if (ATOM(x))
        return x;
    PUSH(x);
    x = CAR(x);
    va = eval ();
    POP(x);
    if (do_break_repl)
        return nil;
    start = lastc = make_cons (va, nil);
    PUSH(start);
    DOLIST(x, CDR(x)) {
        if (ATOM(x)) {
            SETCDR(lastc, x);
            break;
        }
        PUSH(x);
        PUSH(lastc);
        x = CAR(x);
        tmp = eval ();
        POP(lastc);
        SETCDR(lastc, make_cons (tmp, nil));
        lastc = tmp;
        POP(x);
        if (do_break_repl)
            return nil;
    }
    POP(start);
    return start;
}

lispptr
eval0 (void)
{
#ifndef NDEBUG
    char * old_stack    = stack;
    char * old_tagstack = tagstack;
#endif

do_eval:
#ifdef VERBOSE_EVAL
    outs ("-> "); print (x); terpri ();
#endif

#ifdef GC_STRESS
    PUSH(x);
    gc ();
    POP(x);
#endif

#ifndef NO_DEBUGGER
    current_expr = x;
    PUSH(current_expr);
    if (debug_step == t || do_invoke_debugger)
        lisp_repl (REPL_DEBUGGER);
#endif

    if (!x) {
        value = nil;
        goto do_return_atom;
    }

    // Evaluate atom.
    if (ATOM(x)) {
        value = SYMBOLP(x) ? SYMBOL_VALUE(x) : x;
        goto do_return_atom;
    }

    arg1 = CAR(x);
#ifndef NO_DEBUGGER
    PUSH(current_toplevel);
#endif

    if (SYMBOLP(arg1)) {
        if (EXTENDEDP(arg1))
            unevaluated = true;
        arg1 = SYMBOL_VALUE(arg1);
#ifndef NO_DEBUGGER
        if (!unevaluated)
            current_toplevel = x;
#endif
    }

    /////////////////////////
    /// BLOCK name . body ///
    /////////////////////////

    if (arg1 == block_sym) {
#ifndef NAIVE
        if (!CONSP(CDR(x))) {
            error (ERROR_ARG_MISSING, "No name.");
            goto do_return;
        }
#endif
        x = CDR(x);
        arg1 = CAR(x);

#ifndef NAIVE
        if (!SYMBOLP(arg1)) {
            error (ERROR_TYPE, "Name not a sym.");
            goto do_return;
        }
#endif

        arg2c = CDR(x); // Start of expressions.
        value = nil;
        unevaluated = false;
block_statement:
        x = CDR(x);
        if (!x)
            goto do_return;
        PUSH(arg1);     // Block name
        PUSH(arg2c);    // Expressions
        PUSH(x);        // Current expression
        x = CAR(x);
        PUSH_TAG(TAG_NEXT_BLOCK_STATEMENT);
        goto do_eval;
next_block_statement:
        POP(x);
        POP(arg2c);
        POP(arg1);
        if (do_break_repl)
            goto do_return;

        // Handle GO.
        if (value == go_sym) {
            // Search tag in body.
            value = nil;
            tag_found = false;
            DOLIST(x, arg2c)
                if (CAR(x) == go_tag)
                    goto block_statement;
#ifndef NAIVE
            if (!tag_found) {
                error (ERROR_TAG_MISSING, "Tag not found.");
                goto do_return;
            }
#endif
        } else if (value == return_sym) {
            if (arg1 == return_name) {
                value = return_value;
                return_value = nil;
                goto do_return;
            }
            goto do_return;
        }
        goto block_statement;
    }

    args = CDR(x);

    ////////////////
    /// BUILT-IN ///
    ////////////////

    // Call built-in.
    if (BUILTINP(arg1)) {
        bfun = (struct builtin *) SYMBOL_VALUE(arg1);
        badef = bfun->argdef;

        // Built-in without argument-definition.
        if (!badef) {
            // No definition.  Call with unevaluated args.
            x = args;
            value = bfun->func ();
            goto do_return;
        }

        // Call built-in with argument definition.  Pushes
        // the evaluated values onto the stack and pops them
        // into arg1/arg2 before doing the call.

        na = 0;
        PUSH_TAGW(bfun);

do_builtin_arg:
        c = *badef;

        // End of argument definition.
        if (!c) {
#ifndef NAIVE
            // Complain if argument left.
            if (args) {
                error (ERROR_TOO_MANY_ARGS,
                       "Too many args to builtin:");
                goto do_return;
            }
#endif

set_arg_values:
            if (na == 1)
                POP(arg1);
            else if (na == 2) {
                POP(arg2);
                POP(arg1);
            }

            // Call built-in.
            POP_TAGW(bfun);
            value = do_break_repl ? nil : bfun->func ();
            goto do_return;
        }

        na++;

        if (c == '?') {     // Optional argument.
            c = *++badef;
            if (!args) {
                PUSH(nil);
                goto set_arg_values;
            }
        }
#ifndef NAIVE
        else if (!args) { // Missing argument.
            error (ERROR_ARG_MISSING,
                   "Missing arg to builtin.");
            goto do_return;
        }
#endif
        if (c == '\'') {    // Unevaluated argument.
            c = *++badef;
            if (c == '+') { // (Rest of arguments.)
                PUSH(args);
                goto set_arg_values;
            } else
                value = CAR(args);
            goto save_arg_value;
        }
        if (c == '+') {     // Rest of arguments.
            if (unevaluated) {
                value = args;
                goto save_arg_value;
            }
            PUSH(args);
            PUSH_TAG(na);
            x = args;
            value = eval_list ();
#ifndef NAIVE
            if (has_error)
                value = lisp_repl (REPL_DEBUGGER);
#endif
            POP_TAG(na);
            POP(args);

            PUSH(value);
            goto set_arg_values;
        }

        if (unevaluated) {
            value = CAR(args);
            goto save_arg_value;
        }

        // Evaluate argument inline.
        PUSH(args);
        PUSH_TAGW(badef);
        PUSH_TAG(na);
        x = CAR(args);
        PUSH_TAG(TAG_NEXT_BUILTIN_ARG);
        goto do_eval;
        // Step to next argument.
next_builtin_arg:
        POP_TAG(na);
        POP_TAGW(badef);
        POP(args);

        if (do_break_repl) {
            na--;
            goto set_arg_values;
        }

save_arg_value:
#ifndef NAIVE
        bi_tcheck (value, *badef);
        if (has_error) {
            PUSH(args);
            PUSH_TAGW(badef);
            PUSH_TAG(na);
            value = lisp_repl (REPL_DEBUGGER);
            POP_TAG(na);
            POP_TAGW(badef);
            POP(args);
        }
#endif
        if (do_break_repl) {
            na--;
            goto set_arg_values;
        }
        PUSH(value);

        // Step to next argument.
        badef++;
        args = CDR(args);
        goto do_builtin_arg;
    }

    ////////////////////
    /// USER-DEFINED ///
    ////////////////////

    // Call user-defined function.  Saves argument names and
    // their old values on the stack and overwrites the
    // names' symbol values with evaluated ones.  The old
    // values are popped off the stack when the function
    // body has been executed.

#ifndef NAIVE
    // Ensure user-defined function.
    if (ATOM(arg1)) {
        current_expr = arg1;
        error (ERROR_NOT_FUNCTION, "Not a fun");
        goto do_return;
    }
#endif

    // Init argument list evaluation.
    defs = FUNARGS(arg1);
    na = 0;

    // Evaluate argument.
do_argument:
    // End of arguments.
    if (!args && !defs)
        goto start_body;

#ifndef NAIVE
    // Catch wrong number of arguments.
    if (args && !defs) {
        error (ERROR_TOO_MANY_ARGS, "Too many args");
        goto start_body;
    } else if (!args && CONSP(defs)) {
        error (ERROR_ARG_MISSING, "Arg missing");
        goto start_body;
    }
#endif

    na++;

    // Rest of argument list. (consing)
    if (ATOM(defs)) {
        // Save old symbol value for restore_arguments.
        PUSH(SYMBOL_VALUE(defs));

        if (unevaluated)
            value = args;
        else {
            PUSH_TAG(na);
            PUSH(defs);
            PUSH(arg1);
            x = args;
            value = eval_list ();
            POP(arg1);
            POP(defs);
            POP_TAG(na);
        }

        if (!do_break_repl)
            SET_SYMBOL_VALUE(defs, value);
        goto start_body;
    }

    // Save old argument symbol value.
    PUSH(SYMBOL_VALUE(CAR(defs)));

    if (unevaluated)
        value = CAR(args);
    else {
        // Save evaluator state.
        PUSH_TAG(na);
        PUSH(arg1);
        PUSH(defs);
        PUSH(args);

        PUSH_TAG(TAG_NEXT_ARG);
        x = CAR(args);
        goto do_eval;
        // Step to next argument.
next_arg:
        // Restore state.
        POP(args);
        POP(defs);
        POP(arg1);
        POP_TAG(na);
        if (do_break_repl)
            goto start_body;
    }

    // Replace argument symbol value with evaluated one.
    SET_SYMBOL_VALUE(CAR(defs), value);

    // Step to next argument.
    defs = CDR(defs);
    args = CDR(args);
    goto do_argument;

    // Evaluate body.
start_body:
    unevaluated = false;
    PUSH_TAG(na);           // Number of arguments.
    PUSH(FUNARGS(arg1));    // Argument definition.

    x = FUNBODY(arg1);
do_body:
    if (!x || do_break_repl)
        goto restore_arguments;
    PUSH(CDR(x));   // Save next expression.
    x = CAR(x);
    PUSH_TAG(TAG_NEXT_BODY_STATEMENT);
    goto do_eval;
next_body_statement:
    POP(x);
    goto do_body;

restore_arguments:
    // Restore argument symbol values.
    POP(defs);
    POP_TAG(na);
    c = na;
    while (c--) {
        if (ATOM(defs)) {
            if (defs)
                SET_SYMBOL_VALUE(defs, ((lispptr *)stack)[c]);
            break;
        }
        SET_SYMBOL_VALUE(CAR(defs), ((lispptr *)stack)[c]);
        defs = CDR(defs);
    }
    stack += sizeof (lispptr) * na;

do_return:
#ifndef NO_DEBUGGER
    POP(current_toplevel);
#endif
#ifndef NAIVE
    if (has_error)
        value = lisp_repl (REPL_DEBUGGER);
#endif

do_return_atom:
    unevaluated = false;
#ifndef NO_DEBUGGER
    POP(current_expr);
#endif

    if (value == delayed_eval)
        goto do_eval;

#ifdef VERBOSE_EVAL
    outs ("<- "); print (value); terpri ();
#endif

#ifndef NO_DEBUGGER
    if (debug_step && debug_step == current_expr)
        do_invoke_debugger = true;
#endif

    // Dispatch value based on tag.
    POP_TAG(c);
    if (c != TAG_DONE) {
        switch (c) {
        case TAG_NEXT_ARG:
            goto next_arg;
        case TAG_NEXT_BUILTIN_ARG:
            goto next_builtin_arg;
        case TAG_NEXT_BODY_STATEMENT:
            goto next_body_statement;
        case TAG_NEXT_BLOCK_STATEMENT:
            goto next_block_statement;
        }
#ifndef NDEBUG
        internal_error ("Alien tag");
#endif
    }

#ifndef NDEBUG
    check_stacks (old_stack, old_tagstack + 1);
#endif
    return value;
}

lispptr
eval ()
{
    unevaluated = false;
    PUSH_TAG(TAG_DONE);
    return eval0 ();
}

lispptr
funcall ()
{
    unevaluated = true;
    // Tell to return from eval0().
    PUSH_TAG(TAG_DONE);
    return eval0 ();
}
