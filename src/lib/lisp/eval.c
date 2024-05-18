#include <ingle/cc65-charmap.h>

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include <lisp/liblisp.h>
#include <simpleio/libsimpleio.h>

char * stack_start;

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
char * stack;
lispptr ad;
lispptr av;
lispptr name;
lispptr rest;
lispptr value;
builtin_fun bfun;
#ifdef __CC65__
#pragma zpsym ("stack")
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
    cons *  start = NULL;
    cons *  last  = NULL;
    cons *  c;
    lispptr v;

    if (ATOM(x))
        return x;
    while (CONSP(x)) {
        v = eval (CAR(x));
        c = lisp_make_cons (v, nil);
        if (!start)
            start = c;
        if (last) {
            RPLACD(c, last);
            last = c;
        }
        x = CDR(x);
    }
    return start;
}

// Evalue list of expressions and return value of last.
lispptr
eval_body (lispptr x)
{
    lispptr v;

    if (ATOM(x))
        return x;
    for (; CONSP(x); x = CDR(x))
        v = eval (CAR(x));
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
        if (!bfun) {
            lisp_print (fun);
            errouts (": built-in missing.\n\r");
            while (1);
        }
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
            stack -= sizeof (lispptr);
            *(lispptr *) stack = SYMBOL_VALUE(ad);
            stack -= sizeof (lispptr);
            *(lispptr *) stack = ad;

            // Assign rest of arguments to argument symbol.
            rest = do_eval ? eval_list (av) : av;
            SET_SYMBOL_VALUE(ad, rest);
            break;
        }

        // Push argument symbol value on stack.
        name = CAR(ad);
        stack -= sizeof (lispptr);
        *(lispptr *) stack = SYMBOL_VALUE(name);
        stack -= sizeof (lispptr);
        *(lispptr *) stack = name;

        // Assign new value to argument symbol.
        value = do_eval ? eval (CAR(av)) : CAR(av);
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
        name = *(lispptr *) stack;
        stack += sizeof (lispptr);
        SET_SYMBOL_VALUE(name, *(lispptr *) stack);
        stack += sizeof (lispptr);
    }

    return value;
}

lispptr
eval (lispptr x)
{
    symbol * fun;
    lispptr  args;
    if (BUILTINP(x))
        return x;
    if (SYMBOLP(x))
        return SYMBOL_VALUE(x);
    if (ATOM(x))
        return x;
    fun = eval (CAR(x));
    args = CDR(x);
    return apply (fun, args, true);
}
