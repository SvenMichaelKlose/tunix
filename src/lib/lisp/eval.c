#include <ingle/cc65-charmap.h>

#include <stdlib.h>
#include <stdbool.h>

#include <lisp/liblisp.h>
#include <simpleio/libsimpleio.h>

char * stack_start;

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
char * stack;
char * stack_end;
lispptr ad;
lispptr av;
lispptr name;
lispptr rest;
lispptr value;
builtin_fun bfun;
#ifdef __CC65__
#pragma zpsym ("stack")
#pragma zpsym ("stack_end")
#pragma zpsym ("ad")
#pragma zpsym ("av")
#pragma zpsym ("name")
#pragma zpsym ("rest")
#pragma zpsym ("value")
#pragma zpsym ("bfun")
#pragma bss-name (pop)
#endif

// Evaluate list to list of return values.
lispptr
eval_list (lispptr x)
{
    lispptr va;
    lispptr vd;

    if (ATOM(x))
        return x;
    va = eval (CAR(x));
    PUSH(va);
    vd = eval_list (CDR(x));
    POP(va);
    return lisp_make_cons (va, vd);
}

// Evalue list of expressions and return value of last.
lispptr
eval_body (lispptr x)
{
    lispptr v;

    if (ATOM(x))
        return x;
    for (; CONSP(x); x = CDR(x)) {
        PUSH(x);
        v = eval (CAR(x));
        POP(x);
    }
    return v;
}

// Function call
//
// 'fun' is a cons with the argument definition in CAR and
// body expressions in CDR.
lispptr
apply (lispptr fun, lispptr args, bool do_eval)
{
    unsigned stsize;

    if (BUILTINP(fun)) {
        bfun = (builtin_fun) SYMBOL_VALUE(fun);
        return bfun (args);
    }

    if (!CONSP(fun)) {
        errouts ("Function expected, not ");
        lisp_print (fun);
        while (1);
    }

    // Push argument symbol values onto the stack.
    for (ad = FUNARGS(fun), av = args, stsize = 0;
         ad != nil && av != nil;
         ad = CDR(ad), av = CDR(av)) {
        stsize++;

        // Rest of argument list. (consing)
        if (ATOM(ad)) {
            // Push argument symbol value on stack.
            PUSH(SYMBOL_VALUE(ad));
            PUSH(ad);

            // Assign rest of arguments to argument symbol.
            if (do_eval) {
                PUSH(ad);
                PUSH(av);
                value = eval (av);
                POP(av);
                POP(ad);
            } else
                value = av;
            SET_SYMBOL_VALUE(ad, rest);
            break;
        }

        // Push argument symbol value on stack.
        name = CAR(ad);
        PUSH(SYMBOL_VALUE(name));
        PUSH(name);

        // Assign new value to argument symbol.
        if (do_eval) {
            PUSH(ad);
            PUSH(av);
            PUSH(name);
            PUSH(fun);
            value = eval (CAR(av));
            POP(fun);
            POP(name);
            POP(av);
            POP(ad);
        } else
            value = CAR(av);
        SET_SYMBOL_VALUE(name, value);
    }
    if (!NOT(ad)) {
        errouts ("Argument(s) missing: ");
        lisp_print (ad);
        while (1);
    }
    if (!NOT(av)) {
        errouts ("Too many arguments: ");
        lisp_print (av);
        while (1);
    }

    value = eval_body (FUNBODY(fun));

    // Pop argument symbol values from the stack.
    while (stsize--) {
        POP(name);
        POP(SYMBOL_VALUE(name));
    }

    return value;
}

lispptr
eval (lispptr x)
{
    lispptr arg1;

    if (BUILTINP(x))
        return x;
    if (SYMBOLP(x))
        return SYMBOL_VALUE(x);
    if (ATOM(x))
        return x;
    PUSH(x);
    arg1 = eval (CAR(x));
    POP(x);
    return apply (arg1, CDR(x), true);
}
