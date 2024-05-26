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
lispptr value;
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
apply (bool do_eval)
{
    unsigned stsize;

    if (BUILTINP(arg1)) {
        bfun = (builtin_fun) SYMBOL_VALUE(arg1);
        x = args;
        return bfun ();
    }

    if (ATOM(arg1)) {
        errouts ("Function expected, not ");
        lisp_print (arg1);
        lisp_break = true;
        return nil;
    }

    // Push argument symbol values onto the stack.
    for (ad = FUNARGS(arg1), av = args, stsize = 0;
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
                PUSH(arg1);
                value = eval (av);
                POP(arg1);
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
            PUSH(arg1);
            value = eval (CAR(av));
            POP(arg1);
            POP(av);
            POP(ad);
            name = CAR(ad);
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

    // Eavluate body.
    DOLIST(x, FUNBODY(arg1)) {
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
    if (CONSP(x)) {
        PUSH(x);
        arg1 = eval (CAR(x));
        POP(x);
        args = CDR(x);
        return apply (true);
    }
    if (SYMBOLP(x))
        return SYMBOL_VALUE(x);
    return x;
}
