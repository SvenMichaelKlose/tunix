#include <ingle/cc65-charmap.h>

#include <stdlib.h>
#include <stdbool.h>

#include <lisp/liblisp.h>
#include <simpleio/libsimpleio.h>

char * stack_start;

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern lispptr tmp;
lispptr x;
lispptr args;
char * stack;
char * stack_end;
char * tagstack;
lispptr name;
lispptr defs;
lispptr value;
builtin_fun bfun;
lispptr va;
lispptr delayed_eval;
bool lisp_break;
uchar c;
#ifdef __CC65__
#pragma zpsym ("tmp")
#pragma zpsym ("x")
#pragma zpsym ("args")
#pragma zpsym ("stack")
#pragma zpsym ("stack_end")
#pragma zpsym ("tagstack")
#pragma zpsym ("name")
#pragma zpsym ("defs")
#pragma zpsym ("value")
#pragma zpsym ("bfun")
#pragma zpsym ("va")
#pragma zpsym ("delayed_eval")
#pragma zpsym ("lisp_break")
#pragma zpsym ("c")
#pragma bss-name (pop)
#endif

// Evaluate list to list of return values.
lispptr
eval_list (void)
{
    if (lisp_break)
        return nil;
    if (ATOM(x))
        return x;
    PUSH(x);
    x = CAR(x);
    va = eval ();
    POP(x);
    PUSH(va);
    x = CDR(x);
    tmp = eval_list ();
    POP(va);
    return lisp_make_cons (va, tmp);
}

#define PUSH_TAG(x)     (*--tagstack = (x))
#define POP_TAG(x)     ((x) = *tagstack++)

#define TAG_DONE            0
#define TAG_ARG_NEXT        1
#define TAG_ARG_LAST        2
#define TAG_CONTINUE_BODY   3
#define TAG_ARG             4

lispptr
eval0 (void)
{
do_eval:
    // Evaluate atom.
    if (ATOM(x)) {
        if (SYMBOLP(x))
            value = SYMBOL_VALUE(x);
        else
            value = x;
        goto got_value;
    }

    // Evaluate function argument.
    PUSH(x);
    x = CAR(x);
    arg1 = eval ();
    POP(x);
    args = CDR(x);

    // Call built-in with unevaluated arguments.
    if (BUILTINP(arg1)) {
        bfun = (builtin_fun) SYMBOL_VALUE(arg1);
        x = args;
        value = bfun ();
        goto got_value;
    }

    // Complain if not a function.
    if (ATOM(arg1)) {
        errouts ("Function expected, not ");
        lisp_print (arg1);
        lisp_break = true;
        value = nil;
        goto got_value;
    }

    // Init argument list evaluation.
    PUSH_TAG(TAG_DONE);
    defs = FUNARGS(arg1);
    if (!defs && !args)
        goto start_body;

do_argument:
    // Error if lengths of argument list and definition
    // don't match.
    if (!defs || !args) {
        if (defs) {
            errouts ("Argument(s) missing: ");
            lisp_print (defs);
        } else {
            errouts ("Too many arguments: ");
            lisp_print (args);
        }
        lisp_break = true;
        value = nil;
        goto got_value;
    }
 
    // Rest of argument list. (consing)
    if (ATOM(defs)) {
        // Save old symbol value.
        PUSH(SYMBOL_VALUE(defs));
        PUSH(defs);
        PUSH_TAG(TAG_ARG);

        // Evaluate rest of arguments.
        PUSH(defs);
        PUSH(arg1);
        x = args;
        value = eval_list ();
        POP(arg1);
        POP(defs);

        // Assign rest of arguments.
        SET_SYMBOL_VALUE(defs, value);
        goto start_body;
    }

    // Save old argument symbol value.
    name = CAR(defs);
    PUSH(SYMBOL_VALUE(name));
    PUSH(name);
    PUSH_TAG(TAG_ARG);

    PUSH(arg1);
    PUSH(defs);
    if (CDR(defs) || CDR(args)) {
        PUSH(args);
        PUSH_TAG(TAG_ARG_NEXT);
    } else
        PUSH_TAG(TAG_ARG_LAST);
    x = CAR(args);
    goto do_eval;

    // Step to next argument.
next_arg:
    POP(args);
    POP(defs);
    POP(arg1);
    name = CAR(defs);
    SET_SYMBOL_VALUE(name, value);
    defs = CDR(defs);
    args = CDR(args);
    goto do_argument;

    // Handle last argument.
arg_last:
    POP(defs);
    POP(arg1);
    name = CAR(defs);
    SET_SYMBOL_VALUE(name, value);

    // Eavluate body.
start_body:
    x = FUNBODY(arg1);

do_body:
    if (!x || lisp_break)
        goto done_body;
    PUSH(CDR(x));
    PUSH_TAG(TAG_CONTINUE_BODY);
    x = CAR(x);
    goto do_eval;

next_in_body:
    POP(x);
    goto do_body;

done_body:
    // Restore argument symbol values.
    while (POP_TAG(c) == TAG_ARG) {
        POP(name);
        POP(SYMBOL_VALUE(name));
    }
#ifndef NDEBUG
    if (c != TAG_DONE) {
        errouts ("Internal error: ");
        out_number (c);
        outs (": 0 expected after restoring arguments.");
        while (1);
    }
#endif // #ifndef NDEBUG

got_value:
    if (value == delayed_eval)
        goto do_eval;
    POP_TAG(c);
    if (c != TAG_DONE) {
        if (c == TAG_ARG_NEXT)
            goto next_arg;
        if (c == TAG_CONTINUE_BODY)
            goto next_in_body;
        if (c == TAG_ARG_LAST)
            goto arg_last;
//#ifndef NDEBUG
        errouts ("Internal error: ");
        out_number (c);
        outs (": Unknown eval tag.");
        while (1);
//#endif // #ifndef NDEBUG
    }
    return value;
}

lispptr
eval ()
{
    PUSH_TAG(TAG_DONE);
    return eval0 ();
}
