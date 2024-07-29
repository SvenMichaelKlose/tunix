#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#endif

#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

char * stack_start;
lispptr current_expr;
#ifndef NAIVE
lispptr current_function;
lispptr original_first;
#endif

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
lispptr argdefs;
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
char error_code;
#endif
uchar typed_argdef;
char * builtin_argdef;
uchar num_args;
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
#pragma zpsym ("argdefs")
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
#pragma zpsym ("error_code")
#endif
#pragma zpsym ("typed_argdef")
#pragma zpsym ("builtin_argdef")
#pragma zpsym ("num_args")
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

    // Evaluate first element.
    PUSH(x);
    PUSH_HIGHLIGHTED(x);
    x = CAR(x);
    va = eval ();
    POP_HIGHLIGHTED();
    POP(x);

    if (do_break_repl)
        return nil;

    // Make first element of result list and put it on
    // the object stack, so we don't have to worry about
    // garbage collection for the rest of elements.
    PUSH(x);
    list_start = list_last = make_cons (va, nil);
    POP(x);
    PUSH(list_start);

    // Evaluate rest of list.
    DOLIST(x, CDR(x)) {
        // Evaluate CDR of dotted cons.
        if (ATOM(x)) {
            PUSH_HIGHLIGHTED(list_last);
            SETCDR(list_last, eval ());
            POP_HIGHLIGHTED();
            break;
        }

        // Save current cons to object stack.
        PUSH(x);

        // Evaluate CAR of cons.
        PUSH(list_last);
        PUSH_HIGHLIGHTED(x);
        x = CAR(x);
        tmp = eval ();
        POP_HIGHLIGHTED();
        POP(list_last);

        // Make new cons and append it to the end of the
        // result list.
        SETCDR(list_last, make_cons (tmp, nil));
        list_last = tmp;

        // Remember current cons.
        POP(x);

        // Handle break, e.g. because of an error.
        if (do_break_repl) {
            stack += sizeof (lispptr);
            return nil;
        }
    }

    // Return memorized start of result list.
    POP(list_start);
    return list_start;
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

#ifndef NO_DEBUGGER
    current_expr = x;
    PUSH(current_expr);
    if (debug_step == t || do_invoke_debugger) {
        do_invoke_debugger = false;
        lisp_repl (REPL_DEBUGGER);
    }
#endif

    // Evaluate atom.
    if (ATOM(x)) {
        if (x)
            value = SYMBOLP(x) ? SYMBOL_VALUE(x) : x;
        else
            value = nil;
        goto do_return_atom;
    }

    arg1 = CAR(x);
#ifndef NAIVE
    original_first = arg1;
#endif

    // Get function from symbol.
    if (arg1 && SYMBOLP(arg1)) {
        if (EXTENDEDP(arg1))
            unevaluated = true;
        arg1 = SYMBOL_VALUE(arg1);
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

        arg2c = CDR(x); // Expression list
        value = nil;
        unevaluated = false;
block_statement:
        x = CDR(x);
        if (!x)
            goto do_return;
        PUSH(arg1);     // Block name
        PUSH(arg2c);    // Expression list
        PUSH(x);        // Current expression
        PUSH_HIGHLIGHTED(x);
        x = CAR(x);
        PUSH_TAG(TAG_NEXT_BLOCK_STATEMENT);
        goto do_eval;
next_block_statement:
        POP_HIGHLIGHTED();
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

    //////////////////////////
    /// BUILT-IN PROCEDURE ///
    //////////////////////////

    args = CDR(x);

    // Call built-in.
    if (BUILTINP(arg1)) {
        bfun = (struct builtin *) SYMBOL_VALUE(arg1);
        builtin_argdef = (char *) bfun->argdef;

        // Built-in with no argument-definition.
        // Call with arguments unevaluated.
        if (!builtin_argdef) {
            x = args;
            value = bfun->func ();
            goto do_return;
        }

        // Call built-in with argument definition.
        // Push evaluated values on the stack and pop them
        // into arg1/arg2 before doing the call, depending
        // on num_args.
        num_args = 0;
        PUSH_TAGW(bfun);

do_builtin_arg:
        typed_argdef = *builtin_argdef;

        // End of argument definition.
        if (!typed_argdef) {
#ifndef NAIVE
            // Complain if argument left.
            if (args) {
                error (ERROR_TOO_MANY_ARGS,
                       "Too many args to builtin:");
                goto do_return;
            }
#endif

set_arg_values:
            // Pop argument values from object stack into
            // arg1 and arg2, depending on num_args.
            if (num_args == 1)
                POP(arg1);
            else if (num_args == 2) {
                POP(arg2);
                POP(arg1);
            }

            // Call built-in.
            POP_TAGW(bfun);
            value = do_break_repl ? nil : bfun->func ();
            goto do_return;
        }

        num_args++;

        // Optional argument.
        if (typed_argdef == '?') {
            typed_argdef = *++builtin_argdef;

            // Set NIL if missing.
            if (!args) {
                PUSH(nil);
                goto set_arg_values;
            }
        }
#ifndef NAIVE
        // Missing argument error.
        else if (!args) {
            error (ERROR_ARG_MISSING,
                   "Missing arg to builtin.");
            goto do_return;
        }
#endif
        // Unevaluated argument.
        if (typed_argdef == '\'') {
            typed_argdef = *++builtin_argdef;

            // (Rest of arguments.)
            if (typed_argdef == '+') {
                PUSH(args);
                goto set_arg_values;
            } else
                value = CAR(args);
            goto save_arg_value;
        }

        // Rest of arguments.
        if (typed_argdef == '+') {
            if (unevaluated) {
                value = args;
                goto save_arg_value;
            }
            PUSH(args);
            PUSH_TAG(num_args);
            x = args;
            value = eval_list ();
#ifndef NAIVE
            if (error_code)
                value = lisp_repl (REPL_DEBUGGER);
#endif
            POP_TAG(num_args);
            POP(args);

            PUSH(value);
            goto set_arg_values;
        }

        // Save argument unevaluated.
        if (unevaluated) {
            value = CAR(args);
            goto save_arg_value;
        }

        // Evaluate argument inline.
        PUSH(args);
        PUSH_TAGW(builtin_argdef);
        PUSH_TAG(num_args);
        PUSH_HIGHLIGHTED(args);
        x = CAR(args);
        PUSH_TAG(TAG_NEXT_BUILTIN_ARG);
        goto do_eval;
        // Step to next argument.
next_builtin_arg:
        POP_HIGHLIGHTED();
        POP_TAG(num_args);
        POP_TAGW(builtin_argdef);
        POP(args);

        if (do_break_repl)
            goto break_builtin_call;

        // Typecheck and save argument value.
save_arg_value:
#ifndef NAIVE
        // Typecheck,
        bi_tcheck (value, *builtin_argdef);
        if (error_code) {
            PUSH(args);
            PUSH_TAGW(builtin_argdef);
            PUSH_TAG(num_args);
            value = lisp_repl (REPL_DEBUGGER);
            POP_TAG(num_args);
            POP_TAGW(builtin_argdef);
            POP(args);
        }
#endif

        // Break evaluation.
        if (do_break_repl)
            goto break_builtin_call;

        // Save argument value.
        PUSH(value);

        // Step to next argument.
        builtin_argdef++;
        args = CDR(args);
        goto do_builtin_arg;

break_builtin_call:
        num_args--;
        goto set_arg_values;
    }

    //////////////////////////////
    /// USER-DEFINED PROCEDURE ///
    //////////////////////////////

    // Save argument names and their old values on the
    // stack and overwrite the names' symbol values
    // with evaluated ones.  Restore them on return.

#ifndef NAIVE
    // Ensure user-defined function.
    if (ATOM(arg1)) {
        current_expr = arg1;
        error (ERROR_NOT_FUNCTION, "Not a fun");
        goto do_return;
    }
#endif

    // Init argument list evaluation.
    argdefs = FUNARGS(arg1);
    num_args = 0;

    // Evaluate argument.
do_argument:
    // End of arguments.
    if (!args && !argdefs)
        goto start_body;

#ifndef NAIVE
    // Catch wrong number of arguments.
    if (args && !argdefs) {
        error (ERROR_TOO_MANY_ARGS, "Too many args");
        goto start_body;
    } else if (!args && CONSP(argdefs)) {
        error (ERROR_ARG_MISSING, "Arg missing");
        goto start_body;
    }
#endif

    num_args++;

    // Rest of argument list. (consing)
    if (ATOM(argdefs)) {
        // Save old symbol value for restore_arguments.
        PUSH(SYMBOL_VALUE(argdefs));

        // Get argument value.
        if (unevaluated)
            value = args;
        else {
            PUSH_TAG(num_args);
            PUSH(argdefs);
            PUSH(arg1); // Function
#ifndef NAIVE
            PUSH(original_first);
#endif
            x = args;
            value = eval_list ();
#ifndef NAIVE
            POP(original_first);
#endif
            POP(arg1);  // Function
            POP(argdefs);
            POP_TAG(num_args);
        }

        // Save argument value unless we need to fall through.
        if (!do_break_repl)
            SET_SYMBOL_VALUE(argdefs, value);

        goto start_body;
    }

    // Regular argument.  Save its value.
    PUSH(SYMBOL_VALUE(CAR(argdefs)));

    // Get argument value.
    if (unevaluated)
        value = CAR(args);
    else {
        // Save evaluator state.
        PUSH_TAG(num_args);
        PUSH(arg1); // Function
        PUSH(argdefs);
        PUSH(args);
#ifndef NAIVE
        PUSH(original_first);
#endif

        PUSH_TAG(TAG_NEXT_ARG);
        PUSH_HIGHLIGHTED(x);
        x = CAR(args);
        goto do_eval;
        // Step to next argument.
next_arg:
        POP_HIGHLIGHTED();
        // Restore evaluator state.
#ifndef NAIVE
        POP(original_first);
#endif
        POP(args);
        POP(argdefs);
        POP(arg1);  // Function
        POP_TAG(num_args);
        if (do_break_repl)
            goto start_body;
    }

    // Replace argument symbol value with evaluated one.
    SET_SYMBOL_VALUE(CAR(argdefs), value);

    // Step to next argument.
    argdefs = CDR(argdefs);
    args    = CDR(args);
    goto do_argument;

    // Evaluate body.
start_body:
    // Reset inhibited evaluation of special form arguments.
    unevaluated = false;

    // Store init info for restore_arguments.
    PUSH_TAG(num_args);
    PUSH(FUNARGS(arg1));

    // Get first body expression.
    x = FUNBODY(arg1);

#ifndef NAIVE
    // Save function symbol for debugger.
    // TODO: Find out why this cannot be moved before init
    //       info for restore_arguments.
    PUSH(current_function);
    current_function = original_first;
#endif

    // Evaluate body expression.
continue_body:
    if (!x || do_break_repl)
        goto restore_arguments;
    PUSH(CDR(x));   // Next expression.
    PUSH_HIGHLIGHTED(x);
    x = CAR(x);
    PUSH_TAG(TAG_NEXT_BODY_STATEMENT);
    goto do_eval;
next_body_statement:
    POP_HIGHLIGHTED();
    POP(x);
    goto continue_body;

    // Restore argument symbol values.
restore_arguments:
#ifndef NAIVE
    // Restore name of parent function for debugger.
    POP(current_function);
#endif

    // Get argument info.
    POP(argdefs);
    POP_TAG(num_args);
    tmpc = num_args;
    while (tmpc--) {
        if (ATOM(argdefs)) {
            if (argdefs)
                SET_SYMBOL_VALUE(argdefs, ((lispptr *)stack)[(array_index_t) tmpc]);
            break;
        }
        SET_SYMBOL_VALUE(CAR(argdefs), ((lispptr *)stack)[(array_index_t) tmpc]);
        argdefs = CDR(argdefs);
    }
    stack += sizeof (lispptr) * num_args;

do_return:
#ifndef NAIVE
    if (error_code)
        value = lisp_repl (REPL_DEBUGGER);
#endif

do_return_atom:
    unevaluated = false;
#ifndef NO_DEBUGGER
    POP(current_expr);
#endif

    // Evaluate consequence of conditional.
    if (value == delayed_eval)
        goto do_eval;

#ifdef VERBOSE_EVAL
    outs ("<- "); print (value); terpri ();
#endif

#ifndef NO_DEBUGGER
    // Debugger step over expression.
    if (debug_step && debug_step == current_expr)
        // Inoke debugger before next evaluation.
        do_invoke_debugger = true;
#endif

    // Continue evaluation.  Determine jump
    // destination based on tag.
    POP_TAG(typed_argdef);
    if (typed_argdef != TAG_DONE) {
        switch (typed_argdef) {
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

// Call function with arguments unevaluated.
lispptr
eval ()
{
    unevaluated = false;
    PUSH_TAG(TAG_DONE);
    return eval0 ();
}

// Evaluate function call.
lispptr
funcall ()
{
    unevaluated = true;
    PUSH_TAG(TAG_DONE);
    return eval0 ();
}

void
init_eval ()
{
    go_tag = return_name = return_value = nil;
    return_sym   = make_symbol (NULL, 0);
    go_sym       = make_symbol (NULL, 0);
    delayed_eval = make_symbol (NULL, 0);
    block_sym    = make_symbol ("block", 5);
    expand_universe (block_sym);
#ifndef NAIVE
    current_function = nil;
#endif
#ifndef NO_DEBUGGER
    do_invoke_debugger = false;
    debug_step = nil;
#endif
}
