#include <ingle/cc65-charmap.h>

#include <stdlib.h>
#include <stdbool.h>

#include <lisp/liblisp.h>
#include <simpleio/libsimpleio.h>

//#define GC_STRESS

char * stack_start;

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern lispptr tmp;
lispptr x;
lispptr args;
char * stack;
char * stack_end;
lispptr ad;
lispptr av;
lispptr name;
lispptr value;
builtin_fun bfun;
lispptr va;
bool lisp_break;
#ifdef __CC65__
#pragma zpsym ("x")
#pragma zpsym ("args")
#pragma zpsym ("stack")
#pragma zpsym ("stack_end")
#pragma zpsym ("ad")
#pragma zpsym ("av")
#pragma zpsym ("name")
#pragma zpsym ("value")
#pragma zpsym ("bfun")
#pragma zpsym ("lisp_break")
#pragma zpsym ("va")
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
    va = eval (CAR(x));
    POP(x);
    PUSH(va);
    x = CDR(x);
    tmp = eval_list ();
    POP(va);
    return lisp_make_cons (va, tmp);
}

// Function call
//
// 'fun' is a cons with the argument definition in CAR and
// body expressions in CDR.
lispptr
apply (lispptr fun, bool do_eval)
{
    unsigned stsize;

    if (BUILTINP(fun)) {
        bfun = (builtin_fun) SYMBOL_VALUE(fun);
        x = args;
        return bfun ();
    }

    if (!CONSP(fun)) {
        errouts ("Function expected, not ");
        lisp_print (fun);
        lisp_break = true;
        return nil;
    }

    // Push argument symbol values onto the stack.
    for (ad = FUNARGS(fun), av = args, stsize = 0;
         ad && av;
         ad = CDR(ad), av = CDR(av)) {
        stsize++;

        // Rest of argument list. (consing)
        if (ATOM(ad)) {
            // Save argument symbol value.
            PUSH(SYMBOL_VALUE(ad));
            PUSH(ad);

            // Assign rest of arguments to argument symbol.
            if (do_eval) {
                PUSH(ad);
                PUSH(av);
                PUSH(fun);
                value = eval (av);
                POP(fun);
                POP(av);
                POP(ad);
            } else
                value = av;
            SET_SYMBOL_VALUE(ad, value);
            break;
        }

        // Save argument symbol value.
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
    if (ad || av) {
        if (ad) {
            errouts ("Argument(s) missing: ");
            lisp_print (ad);
        } else {
            errouts ("Too many arguments: ");
            lisp_print (av);
        }
        lisp_break = true;
        return nil;
    }

    x = FUNBODY(fun);
    for (; CONSP(x); x = CDR(x)) {
        if (lisp_break)
            break;
        PUSH(x);
        value = eval (CAR(x));
        POP(x);
    }

    // Restore argument symbol values.
    while (stsize--) {
        POP(name);
        POP(SYMBOL_VALUE(name));
    }

#ifdef GC_STRESS
    PUSH(value);
    gc ();
    POP(value);
#endif

    return value;
}

lispptr
eval (lispptr x)
{
    if (BUILTINP(x))
        return x;
    if (SYMBOLP(x))
        return SYMBOL_VALUE(x);
    if (ATOM(x))
        return x;
    PUSH(x);
    arg1 = eval (CAR(x));
    POP(x);
    args = CDR(x);
    return apply (arg1, true);
}
