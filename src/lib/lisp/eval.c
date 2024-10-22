#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#pragma codesize (push, 500)
#endif

#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
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

#ifdef USE_ZEROPAGE
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
char *  tagstack_start;
char *  tagstack;
lispptr argdefs;
struct builtin * bifun;
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
char *  biargdef;
uchar   num_args;
bool    unevaluated;
#ifdef USE_ZEROPAGE
#pragma zpsym ("msg")
#pragma zpsym ("bifun")
#pragma zpsym ("typed_argdef")
#pragma zpsym ("biargdef")
#pragma zpsym ("num_args")
#pragma bss-name (pop)
#endif // #ifdef USE_ZEROPAGE

#ifdef __CC65__
#pragma code-name ("CODE_EVAL")
#endif

// Pop argument values from object stack:
// num_args: Number of arguments.
// argdefs: Argument definition.
void
pop_argument_values (void)
{
    num_args = (argdefs && ATOM(argdefs)) ? 1 : length (argdefs);
    tmpc = num_args;
    while (tmpc--) {
        if (ATOM(argdefs)) { // Rest.
            if (NOT_NIL(argdefs) && SYMBOLP(argdefs))
                SET_SYMBOL_VALUE(argdefs, ((lispptr *)stack)[(array_index_t) tmpc]);
            break;
        }
        if (SYMBOLP(CAR(argdefs)))
            SET_SYMBOL_VALUE(CAR(argdefs), ((lispptr *)stack)[(array_index_t) tmpc]);
        argdefs = CDR(argdefs);
    }
    stack += sizeof (lispptr) * num_args;
}

#ifndef NAIVE
char sp;
#endif

#define TOO_MANY_ARGSP() \
    (NOT_NIL(args) && NOT(argdefs))
#define MISSING_ARGSP() \
    (NOT(args) && CONSP(argdefs))

lispptr
eval0 (void)
{
#ifndef NDEBUG
    char * old_stack    = stack;
    char * old_tagstack = tagstack;
#endif

do_eval:
    // Detach last result.  It may be huge.
    value = nil;

#ifdef VERBOSE_EVAL
    // Print what's about to be evaluated.
    PUSH_TAG(fnout);
    setout (STDOUT);
    outs ("-> "); print (x); terpri ();
    POP_TAG(fnout);
#endif

#ifndef NAIVE
    _GCSTACK_CHECK_OVERFLOW();
    _TAGSTACK_CHECK_OVERFLOW();

#ifdef __CC65__
    // Get the current value of the stack pointer (SP)
    __asm__("tsx");  // Transfer stack pointer to index register X
    __asm__("stx %v", sp);
    if (sp < 8)
        internal_error ("CPU stack");
#endif // #ifdef __CC65__
#endif // #ifndef NAIVE

    //////////
    // ATOM //
    //////////

    if (ATOM(x)) {
        if (NOT_NIL(x))
            value = SYMBOLP(x) ? SYMBOL_VALUE(x) : x;
        else
            value = nil;
        goto return_atom;
    }

    ////////////////
    // EXPRESSION //
    ////////////////

#ifndef NO_DEBUGGER
    PUSH(current_expr);
    current_expr = x;
    arg1 = CAR(x);
#ifndef NAIVE
    unevaluated_arg1 = arg1;
#endif

    // Check if breakpoint.
    tmp = SYMBOL_VALUE(breakpoints_sym);
    if (NOT_NIL(tmp) && member (CAR(x), tmp))
        do_invoke_debugger = true;

    // Call debugger if breakpoint.
    if (do_invoke_debugger || debug_step == t) {
        do_invoke_debugger = false;
        lisp_repl (REPL_DEBUGGER, 0);
    }
#endif // #ifndef NO_DEBUGGER

    // Get function expression from symbol.
    if (NOT_NIL(arg1) && SYMBOLP(arg1)) {
        // Inhinit evaluation of special form's arguments.
        if (EXTENDEDP(arg1))
            unevaluated = true;
        arg1 = SYMBOL_VALUE(arg1);
    }
    args = CDR(x);
    if (arg1 == block_sym)
        goto eval_block;
    if (BUILTINP(arg1))
        goto call_builtin;
    goto call_user_defined;

    /////////////////////////
    /// BLOCK name . body ///
    /////////////////////////
    // Evalue BLOCK inline to avoid using the CPU stack.

eval_block:
#ifndef NAIVE
    if (!CONSP(args)) {
        error (ERROR_NO_BLOCK_NAME, "No name");
        goto return_obj;
    }
#endif
    // Step to and get BLOCK name.
    arg1 = CAR(args);
#ifndef NAIVE
    if (!SYMBOLP(arg1)) {
        error (ERROR_TYPE, "Name not a sym");
        error_info = arg1;
        goto return_obj;
    }
#endif
    arg2c = CDR(args); // Body
    value = nil;
    unevaluated = false;
    x = args;
block_statement:
    // Get first/next statement.
    x = CDR(x);

    // Break on end of body.
    if (NOT(x))
        goto return_obj;

    // Save evaluator state.
    PUSH(arg1);     // Block name
    PUSH(arg2c);    // Body
    PUSH(x);        // Current expression
    HIGHLIGHT(x);
    x = CAR(x);
    PUSH_TAG(TAG_NEXT_BLOCK_STATEMENT);
    goto do_eval;
next_block_statement:
    // Restore evaluator state.
    POP(x);
    POP(arg2c);
    POP(arg1);

    if (do_break_repl)
        goto return_obj;

    if (value == go_sym) {
        // Search tag in body.
        DOLIST(x, arg2c)
            if (CAR(x) == go_tag)
                goto block_statement;
#ifndef NAIVE
        // Tag not found.  Issue error.
        error (ERROR_TAG_MISSING, "Tag not found");
        error_info = go_tag;
        goto return_obj;
#endif
    } else if (value == return_sym) {
        // Handle RETURN,
        if (arg1 == return_name) {
            value = return_value;
            return_value = nil;
        }
        goto return_obj;
    }
    goto block_statement;

    //////////////////////////
    /// BUILT-IN PROCEDURE ///
    //////////////////////////

call_builtin:
    bifun    = (struct builtin *) SYMBOL_VALUE(arg1);
    biargdef = (char *) bifun->argdef;

    // Built-in has no argument-definition.
    // Call it with arguments as they are.
    if (!biargdef) {
        x = args;
        value = bifun->func ();
        goto return_obj;
    }

    num_args = 0;
    PUSH_TAGW(bifun);

do_builtin_arg:
    typed_argdef = *biargdef;

    // End of argument definition.
    if (!typed_argdef) {
#ifndef NAIVE
        // Complain if argument left.
        if (NOT_NIL(args)) {
            error (ERROR_TOO_MANY_ARGS, "Too many args to builtin");
            error_info = args;
            goto return_obj;
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
        POP_TAGW(bifun);
        value = do_break_repl ? nil : bifun->func ();
        goto return_obj;
    }

    // Process an argument.
    num_args++;

    // Optional argument.
    if (typed_argdef == '?') {
        typed_argdef = *++biargdef;

        // Set NIL if missing.
        if (NOT(args)) {
            PUSH(nil);
            goto set_arg_values;
        }
    }
#ifndef NAIVE
    // Missing argument error.
    else if (NOT(args)) {
        error (ERROR_ARG_MISSING, "Missing arg to builtin");
        value = arg1;
        goto return_obj;
    }
#endif
    // Unevaluated argument.
    if (typed_argdef == '\'') {
        typed_argdef = *++biargdef;

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
            PUSH(args);
            goto set_arg_values;
        }

        PUSH(args);
        PUSH_TAG(num_args);
        x = args;
        value = eval_list ();
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
    HIGHLIGHT(args);
    x = CAR(args);
    PUSH(args);
    PUSH_TAGW(biargdef);
    PUSH_TAG(num_args);
    PUSH_TAG(TAG_NEXT_BUILTIN_ARG);
    goto do_eval;
next_builtin_arg:
    POP_TAG(num_args);
    POP_TAGW(biargdef);
    POP(args);

    if (do_break_repl || value == return_sym)
        goto break_builtin_call;

save_arg_value:
#ifndef NAIVE
    // Type-check and throw any errors.
    bi_tcheck (value, *biargdef, ERROR_TYPE);
    if (error_code) {
        PUSH(args);
        PUSH_TAGW(biargdef);
        PUSH_TAG(num_args);
        value = lisp_repl (REPL_DEBUGGER, 0);
        POP_TAG(num_args);
        POP_TAGW(biargdef);
        POP(args);
        if (do_break_repl)
            goto break_builtin_call;
    }
#endif
    // Break evaluation.
    if (value == return_sym)
        goto break_builtin_call;

    // Save argument value.
    PUSH(value);

    // Step to next argument and its definition.
    biargdef++;
    args = CDR(args);
    goto do_builtin_arg;

break_builtin_call:
    num_args--;
    goto set_arg_values;

    //////////////////////////////
    /// USER-DEFINED PROCEDURE ///
    //////////////////////////////

call_user_defined:
#ifndef NAIVE
    // Ensure user-defined function.
    if (ATOM(arg1)) {
        error (ERROR_NOT_FUNCTION, "Not a fun");
        error_info = unevaluated_arg1;
        goto return_obj;
    }
#endif

    // Push old symbol values on the stack,
    argdefs = FUNARGS(arg1);
    while (NOT_NIL(argdefs)) {
        if (ATOM(argdefs)) { // Rest.
            if (SYMBOLP(argdefs))
                PUSH(SYMBOL_VALUE(argdefs));
            else
                PUSH(nil);
            break;
        }
        if (SYMBOLP(CAR(argdefs)))
            PUSH(SYMBOL_VALUE(CAR(argdefs)));
        else
            PUSH(nil);
        argdefs = CDR(argdefs);
    }

    argdefs = FUNARGS(arg1);

do_argument:
    // Arguments complete?
    if (NOT(args) && NOT(argdefs))
        goto start_body;

#ifndef NAIVE
    // Catch wrong number of arguments.
    if (TOO_MANY_ARGSP()) {
        error (ERROR_TOO_MANY_ARGS, "Too many args");
        error_info = args;
        goto start_body;
    } else if (MISSING_ARGSP()) {
        error (ERROR_ARG_MISSING, "Missing args");
        error_info = argdefs;
        goto break_user_call;
    }
#endif

    // Rest argument.
    if (ATOM(argdefs)) {
#ifndef NAIVE
        if (!SYMBOLP(argdefs))
            error_argname (argdefs);
#endif

        if (unevaluated)
            value = args;
        else {
            PUSH(arg1);
            x = args;
            value = eval_list ();
            POP(arg1);
        }
        PUSH(value);
        goto start_body;
    }

    // Regular argument.
#ifndef NAIVE
    // Check if name is a symbol.
    if (!SYMBOLP(CAR(argdefs))) {
        error_argname (CAR(argdefs));
        goto break_user_call;
    }
#endif

    // Get argument value.
    if (unevaluated)
        value = CAR(args);
    else {
        HIGHLIGHT(args);
        x = CAR(args);
        PUSH(arg1); // Function
        PUSH(argdefs);
        PUSH(args);
        PUSH_TAG(TAG_NEXT_ARG);
        goto do_eval;
next_arg:
        POP(args);
        POP(argdefs);
        POP(arg1);  // Function
        if (do_break_repl || value == return_sym)
            goto break_user_call;
    }
    PUSH(value);

    // Step to next argument.
    argdefs = CDR(argdefs);
    args    = CDR(args);
    goto do_argument;

break_user_call:
    // Early break: Fill up argument list to expected length.
    stack -= sizeof (lispptr) * ((argdefs && ATOM(argdefs)) ? 1 : length (argdefs));

start_body:
    argdefs = FUNARGS(arg1);
#ifndef NAIVE
    if (do_break_repl || value == return_sym) {
        stack += sizeof (lispptr) * ((argdefs && ATOM(argdefs)) ? 1 : length (argdefs));
        goto restore_arguments_break;
    }
#endif
    pop_argument_values ();
    unevaluated = false;
    PUSH(FUNARGS(arg1));
    x = FUNBODY(arg1);
#ifndef NO_DEBUGGER
    PUSH(current_function);
    if (NOT_NIL(unevaluated_arg1) && _SYMBOLP(unevaluated_arg1))
        current_function = unevaluated_arg1;
#endif

continue_body:
    if (NOT(x) || do_break_repl)
        goto restore_arguments;
    PUSH(CDR(x));   // Next expression.
    HIGHLIGHT(x);
    x = CAR(x);
    PUSH_TAG(TAG_NEXT_BODY_STATEMENT);
    goto do_eval;
next_body_statement:
    POP(x);
#ifndef NAIVE
    if (value != return_sym && value != go_sym)
#endif
        goto continue_body;

    // Restore argument symbol values.
restore_arguments:
#ifndef NO_DEBUGGER
    POP(current_function);
#endif
    POP(argdefs);
#ifndef NAIVE
restore_arguments_break:
#endif
    pop_argument_values ();

    //////////////
    /// RETURN ///
    //////////////

return_obj:
#ifndef NAIVE
    if (error_code)
        value = lisp_repl (REPL_DEBUGGER, 0);
#endif
#ifndef NO_DEBUGGER
    POP(current_expr);
#endif

return_atom:
    unevaluated = false;

#ifndef NO_DEBUGGER
    // Invoke debugger if we stepped over expression.
    if (debug_step && debug_step == current_expr)
        do_invoke_debugger = true;
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
        internal_error_ptr (tagstack, "return tag");
#endif
    }
#ifndef NDEBUG
    // Check if stacks are in balance.
    check_stacks (old_stack, old_tagstack + 1);
#endif
    return value;
}

// Evaluate list to list of return values.
// TODO: Inline into eval0.
lispptr
eval_list (void)
{
    if (ATOM(x))
        return x;

    // Evaluate first element.
    PUSH(x);
    HIGHLIGHT(x);
    x = CAR(x);
    eval ();
    POP(x);

    if (do_break_repl || value == return_sym)
        return value;

    // Make first element of result list and put it on
    // the object stack, so we don't have to worry about
    // garbage collection for the rest of elements.
    list_start = list_last = make_cons (value, nil);
    PUSH(list_start);

    // Evaluate rest of list.
    DOLIST(x, CDR(x)) {
        // Evaluate CDR of dotted cons.
        if (ATOM(x)) {
            HIGHLIGHT(list_last);
            SETCDR(list_last, eval ());
            break;
        }

        // Save current cons to object stack.
        PUSH(x);

        // Evaluate CAR of cons.
        PUSH(list_last);
        HIGHLIGHT(x);
        x = CAR(x);
        eval ();
        POP(list_last);

        // Make new cons and append it to the end of the
        // result list.
        tmp = make_cons (value, nil);
        SETCDR(list_last, tmp);
        list_last = tmp;
        tmp = nil;

        // Remember current cons.
        POP(x);

        // Handle break, e.g. because of an error.
        if (do_break_repl || value == return_sym) {
            stack += sizeof (lispptr);
            return value;
        }
    }

    // Return memorized start of result list.
    POP(list_start);
    return list_start;
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
#pragma codesize (pop)
#pragma code-name ("CODE_INIT")
#pragma inline-stdfuncs (off)
#pragma allow-eager-inline (off)
#endif

extern lispptr needle;

void
init_eval ()
{
    go_tag = return_name = return_value = nil;
    args = argdefs = arg1 = arg2 = arg2c = nil;
    x = value = needle = nil;

#ifdef NDEBUG
    return_sym   = make_symbol (NULL, 0);
#else
    return_sym   = make_symbol ("~RETURN", 7);
#endif
#ifdef NDEBUG
    go_sym       = make_symbol (NULL, 0);
#else
    go_sym       = make_symbol ("~GO", 3);
#endif
#ifdef NDEBUG
    delayed_eval = make_symbol (NULL, 0);
#else
    delayed_eval = make_symbol ("~EVAL", 5);
#endif
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
