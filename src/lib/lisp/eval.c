#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#endif

#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <setjmp.h>
#ifdef TARGET_UNIX
#include <signal.h>
#endif

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

char *  stack_end;
char *  tagstack_end;

lispptr current_expr;
#ifndef NAIVE
lispptr current_function;
#endif

#ifndef NO_DEBUGGER
lispptr breakpoints_sym;
#endif

#ifndef NAIVE
char    error_code;
lispptr error_info;
#endif

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern char * msg;
lispptr x;
lispptr args;
#ifndef NAIVE
lispptr unevaluated_arg1;
#endif
lispptr arg1;
lispptr arg2c;
lispptr arg2;
lispptr value;
char *  stack;
char *  stack_start;
char *  stack_old_arg_values;
char *  stack_entered;
char *  tagstack_start;
char *  tagstack;
lispptr argdefs;
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
bool    do_invoke_debugger;
uchar   typed_argdef;
char *  builtin_argdef;
uchar   num_args;
bool    unevaluated;
#ifdef __CC65__
#pragma zpsym ("msg")
#pragma zpsym ("stack_entered")
#pragma zpsym ("bfun")
#pragma zpsym ("va")
#pragma zpsym ("typed_argdef")
#pragma zpsym ("builtin_argdef")
#pragma zpsym ("num_args")
#pragma bss-name (pop)
#endif

#ifdef __CC65__
#pragma code-name ("CODE_EVAL")
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
        tmp = make_cons (tmp, nil);
        SETCDR(list_last, tmp);
        list_last = tmp;
        tmp = nil;

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

// Pop argument values from object stack:
// num_args: Number of arguments.
// argdefs: Argument definition.
void
pop_argument_values (void)
{
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
    // Print what's about to be evaluated.
    PUSH_TAG(fnout);
    setout (STDOUT);
    outs ("-> "); print (x); terpri ();
    POP_TAG(fnout);
#endif
#ifndef NO_DEBUGGER
    // Inovke debugger.
    PUSH(current_expr);
    current_expr = x;
#ifndef NO_DEBUGGER
        if (CONSP(x) && SYMBOLP(CAR(x)) && member (CAR(x), SYMBOL_VALUE(breakpoints_sym)))
            do_invoke_debugger = true;
#endif
    if (do_invoke_debugger || debug_step == t) {
        do_invoke_debugger = false;
        lisp_repl (REPL_DEBUGGER);
    }
#endif
    // Evaluate atom.
    if (ATOM(x)) {
        if (x) // (NOT_NIL(x)) surfaces https://github.com/cc65/cc65/issues/2487
            value = SYMBOLP(x) ? SYMBOL_VALUE(x) : x;
        else
            value = nil;
        goto do_return_atom;
    }

    // Get first element, which is the procedure to call.
    arg1 = CAR(x);
#ifndef NAIVE
    unevaluated_arg1 = arg1;
#endif
    // Get function expression from symbol.
    if (NOT_NIL(arg1) && SYMBOLP(arg1)) {
        // Inhinit evaluation of special form's arguments.
        if (EXTENDEDP(arg1))
            unevaluated = true;
        arg1 = SYMBOL_VALUE(arg1);
    }

    /////////////////////////
    /// BLOCK name . body ///
    /////////////////////////
    // Evalue BLOCK inline to avoid using the CPU stack.

    if (arg1 == block_sym) {
#ifndef NAIVE
        if (!CONSP(CDR(x))) {
            error (ERROR_NO_BLOCK_NAME, "No name");
            goto do_return;
        }
#endif
        // Step to and get BLOCK name.
        x = CDR(x);
        arg1 = CAR(x);
#ifndef NAIVE
        if (!SYMBOLP(arg1)) {
            error_info = arg1;
            error (ERROR_TYPE, "Name not a sym");
            goto do_return;
        }
#endif
        // Get body.
        arg2c = CDR(x);

        // Return value of empty BLOCK.
        value = nil;

        unevaluated = false;
block_statement:
        // Get first/next statement.
        x = CDR(x);

        // Break on end of body.
        if (NOT(x))
            goto do_return;

        // Save evaluator state.
        PUSH(arg1);     // Block name
        PUSH(arg2c);    // Body
        PUSH(x);        // Current expression
        PUSH_HIGHLIGHTED(x);
        x = CAR(x);
        PUSH_TAG(TAG_NEXT_BLOCK_STATEMENT);
        goto do_eval;
next_block_statement:
        // Restore evaluator state.
        POP_HIGHLIGHTED();
        POP(x);
        POP(arg2c);
        POP(arg1);

        // Return to break REPL if demanded.
        if (do_break_repl)
            goto do_return;

        if (value == go_sym) {
            // Handle GO.
            value = nil;

            // Search tag in body.
            DOLIST(x, arg2c)
                if (CAR(x) == go_tag)
                    goto block_statement;
#ifndef NAIVE
            // Tag not found.  Issue error.
            error_info = go_tag;
            error (ERROR_TAG_MISSING, "Tag not found");
            goto do_return;
#endif
        } else if (value == return_sym) {
            // Handle RETURN,
            if (arg1 == return_name) {
                value = return_value;
                return_value = nil;
                goto do_return;
            }
            goto do_return;
        }
        goto block_statement;
    }

    // Get argument list following the procedure's name.
    args = CDR(x);

    //////////////////////////
    /// BUILT-IN PROCEDURE ///
    //////////////////////////

    if (BUILTINP(arg1)) {
        bfun = (struct builtin *) SYMBOL_VALUE(arg1);
        builtin_argdef = (char *) bfun->argdef;

        // Built-in has no argument-definition.
        // Call it with arguments as they are.
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
            if (NOT_NIL(args)) {
                error_info = args;
                error (ERROR_TOO_MANY_ARGS, "Too many args to builtin");
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
            if (NOT(args)) {
                PUSH(nil);
                goto set_arg_values;
            }
        }
#ifndef NAIVE
        // Missing argument error.
        else if (NOT(args)) {
            error_info = unevaluated_arg1;
            error (ERROR_ARG_MISSING, "Missing arg to builtin");
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

save_arg_value:
#ifndef NAIVE
        // Type-check and throw any errors.
        bi_tcheck (value, *builtin_argdef, ERROR_TYPE);
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

        // Step to next argument and its definition.
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
        error_info = arg1;
        error (ERROR_NOT_FUNCTION, "Not a fun");
        goto do_return;
    }
#endif
    // Save stack pointer start.
    stack_entered = stack;

    // Save stack position of old argument symbol values.
    stack_old_arg_values = stack;

    // Get argument definition and number of arguments.
    argdefs = FUNARGS(arg1);
    num_args = (argdefs && ATOM(argdefs)) ? 1 : length (argdefs);

    // NIL out stack for old argument symbol values,
    // so garbage collection won't crash.
    tmpc = num_args;
    while (tmpc--)
        PUSH(nil);

    // Evaluate argument.
do_argument:
    // End of arguments.
    if (NOT(args) && NOT(argdefs))
        goto start_body;

#ifndef NAIVE
    // Catch wrong number of arguments.
    if (NOT_NIL(args) && NOT(argdefs)) {
        error_info = args;
        error (ERROR_TOO_MANY_ARGS, "Too many args");
        goto start_body;
    } else if (NOT(args) && CONSP(argdefs)) {
        error_info = argdefs;
        error (ERROR_ARG_MISSING, "Missing args");
        goto start_body;
    }
#endif
    // Rest of argument list. (consing)
    if (ATOM(argdefs)) {
#ifndef NAIVE
        // Check if argument name is a symbol.
        if (!SYMBOLP(argdefs))
            error_argname (argdefs);
#endif
        // Save old symbol value for restore_arguments.
        stack_old_arg_values -= sizeof (lispptr);
        *(lispptr *) stack_old_arg_values = SYMBOL_VALUE(argdefs);

        // Get argument value.
        if (unevaluated)
            value = args;
        else {
            // Save evaluator state.
            PUSH_TAGW(stack_entered);
            PUSH_TAGW(stack_old_arg_values);
            PUSH_TAG(num_args);
            PUSH(argdefs);
            PUSH(arg1);
#ifndef NAIVE
            PUSH(unevaluated_arg1);
#endif
            // Evaluate rest of arguments.
            x = args;
            value = eval_list ();

            // Restore evaluator state.
#ifndef NAIVE
            POP(unevaluated_arg1);
#endif
            POP(arg1);
            POP(argdefs);
            POP_TAG(num_args);
            POP_TAGW(stack_old_arg_values);
            POP_TAGW(stack_entered);
        }

        // Save argument value unless we need to fall through.
        if (!do_break_repl)
            PUSH(value);

        goto start_body;
    }

    // Regular argument.
#ifndef NAIVE
    // Check if name is a symbol.
    if (!SYMBOLP(CAR(argdefs)))
        error_argname (CAR(argdefs));
#endif
    // Save argument value to restore after function call.
    stack_old_arg_values -= sizeof (lispptr);
    *(lispptr *) stack_old_arg_values = SYMBOL_VALUE(CAR(argdefs));

    // Get argument value.
    if (unevaluated)
        value = CAR(args);
    else {
        // Save evaluator state.
        PUSH_TAGW(stack_entered);
        PUSH_TAGW(stack_old_arg_values);
        PUSH_TAG(num_args);
        PUSH(arg1); // Function
        PUSH(argdefs);
        PUSH(args);
#ifndef NAIVE
        PUSH(unevaluated_arg1);
#endif
        PUSH_TAG(TAG_NEXT_ARG);
        PUSH_HIGHLIGHTED(args);
        x = CAR(args);
        goto do_eval;
next_arg:
        // Restore evaluator state.
        POP_HIGHLIGHTED();
#ifndef NAIVE
        POP(unevaluated_arg1);
#endif
        POP(args);
        POP(argdefs);
        POP(arg1);  // Function
        POP_TAG(num_args);
        POP_TAGW(stack_old_arg_values);
        POP_TAGW(stack_entered);
        if (do_break_repl)
            goto start_body;
    }

    // Save new argument symbol value.
    PUSH(value);

    // Step to next argument.
    argdefs = CDR(argdefs);
    args    = CDR(args);
    goto do_argument;

start_body:
#ifndef NAIVE
    if (error_code || do_break_repl) {
        stack = stack_entered;
        goto do_return;
    }
#endif
    // Assign new values to argument symbols.
    argdefs = FUNARGS(arg1);
    pop_argument_values ();

    // Reset inhibited evaluation of special form arguments.
    unevaluated = false;

    // Store init info for restore_arguments.
    PUSH_TAG(num_args);
    PUSH(FUNARGS(arg1));

    // Get first body expression.
    x = FUNBODY(arg1);

#ifndef NO_DEBUGGER
    // Save function symbol for debugger.
    // TODO: Find out why this cannot be moved before init
    //       info for restore_arguments.
    PUSH(current_function);
    if (ATOM(unevaluated_arg1))
        current_function = unevaluated_arg1;
#endif
continue_body:
    // Evaluate body expression.
    if (NOT(x) || do_break_repl)
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
#ifndef NO_DEBUGGER
    // Restore name of parent function for debugger.
    POP(current_function);
#endif
    POP(argdefs);
    POP_TAG(num_args);
    pop_argument_values ();

do_return:
#ifndef NAIVE
    if (error_code)
        value = lisp_repl (REPL_DEBUGGER);
#endif
do_return_atom:
    unevaluated = false;

#ifndef NO_DEBUGGER
    // Invoke debugger if we stepped over this expression.
    if (debug_step && debug_step == current_expr)
        // Inoke debugger before next evaluation.
        do_invoke_debugger = true;
#endif

#ifndef NO_DEBUGGER
    POP(current_expr);
#endif
    // Evaluate consequence of conditional.
    if (value == delayed_eval)
        goto do_eval;
#ifdef VERBOSE_EVAL
    // Print return value.
    PUSH_TAG(fnout);
    setout (STDOUT);
    outs ("<- "); print (value); terpri ();
    POP_TAG(fnout);
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
        internal_error_ptr (tagstack, "alien tag");
#endif
    }
#ifndef NDEBUG
    // Check if stacks are in balance.
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

#ifdef __CC65__
#pragma code-name ("CODE_INIT")
#pragma inline-stdfuncs (off)
#pragma allow-eager-inline (off)
#endif

extern lispptr needle;

void
init_eval ()
{
    go_tag = return_name = return_value = nil;
    args = argdefs = arg1 = arg2 = arg2c = x = value = va = nil;
    needle = nil;
    return_sym   = make_symbol (NULL, 0);
    go_sym       = make_symbol (NULL, 0);
    delayed_eval = make_symbol (NULL, 0);
    block_sym    = make_symbol ("block", 5);
    expand_universe (block_sym);
#ifndef NAIVE
    current_function = nil;
#endif
#ifndef NO_DEBUGGER
    breakpoints_sym = make_symbol ("*b*", 3);
    expand_universe (breakpoints_sym);
    SET_SYMBOL_VALUE(breakpoints_sym, nil);
    do_invoke_debugger = false;
    debug_step = nil;
    error_info = nil;
#endif
}
