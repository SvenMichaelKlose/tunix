#include <ingle/cc65-charmap.h>

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include <term/libterm.h>
#include <lisp/liblisp.h>

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
lispptr * stack;
#ifdef __CC65__
#pragma zpsym ("stack")
#pragma bss-name (pop)
#endif

#define PUSH(x)     (*--stack = x)
#define POP(x)      (x = *++stack)
#define PUSHSIZE(x) PUSH((lispptr) x)
#define POPSIZE(x)  (x = (unsigned) *++stack)

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
    lispptr argdef = FUNARGS(fun);
    lispptr ad;
    lispptr av;
    lispptr name;
    //lispptr rest;
    lispptr value;
    builtin_fun bfun;
    unsigned stsize;

    if (BUILTINP(fun)) {
        bfun = (builtin_fun) SYMBOL_VALUE(fun);
        if (!bfun) {
            lisp_print (fun);
            term_puts (": built-in missing.\n\r");
            while (1);
        }
        return bfun (args);
    }

    if (!CONSP(fun)) {
        term_puts ("Function expected, not ");
        lisp_print (fun);
        while (1);
    }

    // Push argument symbol values onto the stack.
    argdef = FUNARGS(fun);
    for (ad = argdef, av = args, stsize = 0;
         ad != nil && av != nil;
         ad = CDR(ad), av = CDR(av)) {
        stsize++;

        // Rest of argument list. (consing)
        /*
        if (ATOM(ad)) {
            PUSH(ad);
            PUSH(SYMBOL_VALUE(ad));
            rest = do_eval ? eval_list (av) : av;
            SET_SYMBOL_VALUE(ad, rest);
            break;
        }
        */

        name = CAR(ad);
        //PUSH(name);
        //PUSH(SYMBOL_VALUE(name));
        value = do_eval ? eval (CAR(av)) : CAR(av);
        SET_SYMBOL_VALUE(name, value);
    }

    //PUSHSIZE(stsize);
    value = eval_body (FUNBODY(fun));
    //POPSIZE(stsize);

    // Pop argument symbol values from the stack.
    /*
    while (stsize--) {
        POP(name);
        POP(SYMBOL_VALUE(name));
    }
    */

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
