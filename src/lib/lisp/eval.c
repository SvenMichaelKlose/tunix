#include <ingle/cc65-charmap.h>

#include <stdlib.h>
#include <stdbool.h>

#include <lisp/liblisp.h>
#include <simpleio/libsimpleio.h>

extern void error (char *);

char * stack_start;

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern lispptr tmp;
extern char * msg;
lispptr x;
lispptr args;
char * stack;
char * stack_end;
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
bool tag_found;
bool lisp_break;
uchar c;
char * badef;
uchar na;
bool unevaluated;
#ifdef __CC65__
#pragma zpsym ("tmp")
#pragma zpsym ("msg")
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
#pragma zpsym ("block_sym")
#pragma zpsym ("return_sym")
#pragma zpsym ("return_name")
#pragma zpsym ("return_value")
#pragma zpsym ("go_sym")
#pragma zpsym ("go_tag")
#pragma zpsym ("tag_found")
#pragma zpsym ("lisp_break")
#pragma zpsym ("c")
#pragma zpsym ("badef")
#pragma zpsym ("na")
#pragma zpsym ("unevaluated")
#pragma bss-name (pop)
#endif

extern void bierror (void);

// Type-check object.
void
bi_tcheck (lispptr x, uchar type)
{
    (void) x, (void) type;

    switch (type) {
    case 'x': // anything
        return;

    case 'n': // number
        if (!NUMBERP(x)) {
            msg = "Number expected.";
            bierror ();
            while (1);
        }
        return;

    case 's': // symbol
        if (!SYMBOLP(x)) {
            msg = "Symbol expected.";
            bierror ();
            while (1);
        }
        return;

    case 'c': // cons
        if (!CONSP(x)) {
            msg = "Cons expected.";
            bierror ();
            while (1);
        }
        return;

    case 'l': // list (cons or nil)
        if (!LISTP(x)) {
            msg = "List expected.";
            bierror ();
            while (1);
        }
        return;

    case 'f': // function
        if (!LISTP(x) && !BUILTINP(x)) {
            msg = "Function expected.";
            bierror ();
            while (1);
        }
        return;

#ifndef NDEBUG
    default:
        outs ("Developer error: '");
        out (type);
        outs ("': unknown typedef");
        while (1);
#endif
    }
}

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
#define POP_TAG(x)      ((x) = *tagstack++)
#define PUSH_TAGW(x) \
    do { \
        tagstack -= sizeof (lispptr); \
        *(lispptr *) tagstack = x; \
    } while (0)
#define POP_TAGW(x) \
    do { \
        x = *(lispptr *) tagstack; \
        tagstack += sizeof (lispptr); \
    } while (0)

#define TAG_DONE            0
#define TAG_BARG_NEXT       1
#define TAG_ARG_NEXT        2
#define TAG_CONTINUE_BODY   3
#define TAG_CONTINUE_BLOCK  4
#define TAG_ARG             5

lispptr
eval0 (void)
{
do_eval:
    if (!x) {
        value = nil;
        goto got_value;
    }

    // Evaluate atom.
    if (ATOM(x)) {
        value = SYMBOLP(x) ? SYMBOL_VALUE(x) : x;
        goto got_value;
    }

    // Evaluate function.
    PUSH(x);
    x = CAR(x);
    arg1 = eval ();
    POP(x);

    // Do BLOCK.
    if (arg1 == block_sym) {
        x = CDR(x);
        if (!CONSP(x)) {
            msg = "No name.";
            bierror ();
        }
        arg1 = CAR(x);

        if (!SYMBOLP(arg1)) {
            msg = "Name not a sym.";
            bierror ();
        }
        arg2c = CDR(x);

        value = nil;
block_statement:
        if (lisp_break)
            goto got_value;
        x = CDR(x);
        if (!x)
            goto got_value;
        PUSH(arg1);
        PUSH(arg2c);
        PUSH(x);
        x = CAR(x);
        PUSH_TAG(TAG_CONTINUE_BLOCK);
        goto do_eval;
next_block_statement:
        POP(x);
        POP(arg2c);
        POP(arg1);

        // Handle GO.
        if (value == go_sym) {
            // Search tag in body.
            value = nil;
            tag_found = false;
            TYPESAFE_DOLIST(x, arg2c)
                if (CAR(x) == go_tag)
                    goto block_statement;
            if (!tag_found) {
                error ("Tag not found.");
                goto got_value;
            }
        }

        if (value == return_sym) {
            if (arg1 == return_name) {
                value = return_value;
                return_value = nil;
                goto got_value;
            }
            goto got_value;
        }
        goto block_statement;
    }

    args = CDR(x);

    // Call built-in.
    if (BUILTINP(arg1)) {
        bfun = (struct builtin *) SYMBOL_VALUE(arg1);
        badef = bfun->argdef;

        // Built-in without argument-definition.
        if (!badef) {
            // No definition.  Call with unevaluated args.
            x = args;
            value = bfun->func ();
            goto got_value;
        }

// Call built-in with argument definition.  Pushes the
// evaluated values onto the stack and pops them into
// arg1/arg2 before doing the call.

        na = 0;
        PUSH_TAGW(bfun);

do_builtin_arg:
        c = *badef;

        // End of argument definition.
        if (!c) {
            // Complain if argument left.
            if (args) {
                msg = "Too many args to builtin:";
                bierror ();
                lisp_print (args);
                while (1);
                goto got_value;
            }

            if (na == 1)
                POP(arg1);
            else if (na == 2) {
                POP(arg2);
                POP(arg1);
            }

            // And call the built-in...
            POP_TAGW(bfun);
            value = bfun->func ();
            goto got_value;
        }

        // Complain about missing argument.
        if (!args) {
            msg = "Missing args to builtin.";
            bierror ();
            goto got_value;
        }

        // Now be have the argument in the head of 'args'
        // and its wanted type in *badef.  Increment the
        // argument count.
        na++;

        // Quick deal with unevaluated argument.
        if (c == '\'') {
            badef++;
            value = CAR(args);
            goto save_builtin_arg_value;
        }

        // Evaluate argument inline.
        PUSH(args);
        PUSH_TAGW(badef);
        PUSH_TAG(na);
        x = CAR(args);
        PUSH_TAG(TAG_BARG_NEXT);
        goto do_eval;
        // Step to next argument.
next_builtin_arg:
        POP_TAG(na);
        POP_TAGW(badef);
        POP(args);

save_builtin_arg_value:
        // Ensure the type is wanted.
        bi_tcheck (value, *badef++);

        // Save evaluated value on the GC stack to move it
        // to 'arg1' and 'arg2' when finished with all
        // arguments.
        PUSH(value);

        // Step to next argument.
        args = CDR(args);
        goto do_builtin_arg;
    }

// Call user-defined function.  Saves argument names and
// their old values on the stack and overwrites the names'
// symbol values with evaluated ones.  The old values are
// popped off the stack when the function body has been
// executed.

    // Ensure user-defined function.
    if (ATOM(arg1)) {
        error ("Function expected, not: ");
        value = nil;
        goto got_value;
    }

    // Init argument list evaluation.
    PUSH_TAG(TAG_DONE);
    defs = FUNARGS(arg1);

    // Evaluate arguments to user-defined function.
do_argument:
    // End of arguments.
    if (!defs || !args) {
        if (defs) {
            errouts ("Argument(s) missing: ");
            lisp_print (defs);
        } else if (args) {
            errouts ("Too many arguments: ");
            lisp_print (args);
        }
        goto start_body;
    }
 
    // Rest of argument list. (consing)
    if (ATOM(defs)) {
        // Save old symbol value.
        PUSH(SYMBOL_VALUE(defs));
        PUSH(defs);
        PUSH_TAG(TAG_ARG);

        if (unevaluated)
            value = x;
        else {
            // Evaluate rest of arguments.
            PUSH(defs);
            PUSH(arg1);
            x = args;
            value = eval_list ();
            POP(arg1);
            POP(defs);
        }

        // Assign rest of arguments.
        SET_SYMBOL_VALUE(defs, value);
        goto start_body;
    }

    // Save old argument symbol value.
    name = CAR(defs);
    PUSH(SYMBOL_VALUE(name));
    PUSH(name);
    PUSH_TAG(TAG_ARG);

    if (unevaluated)
        value = CAR(args);
    else {
        // Save evaluator state.
        PUSH(arg1);
        PUSH(defs);
        PUSH(args);

        PUSH_TAG(TAG_ARG_NEXT);
        x = CAR(args);
        goto do_eval;
        // Step to next argument.
next_arg:
        // Restore state.
        POP(args);
        POP(defs);
        POP(arg1);
    }

    // Replace argument symbol value with evaluated one.
    name = CAR(defs);
    SET_SYMBOL_VALUE(name, value);

    // Step to next argument.
    defs = CDR(defs);
    args = CDR(args);
    goto do_argument;

    // Evaluate body.
start_body:
    x = FUNBODY(arg1);

    // Evaluate body statement.
do_body:
    // Break if out of statements or other reason.
    if (!x || lisp_break)
        goto body_done;

    // Save rest of statements on the GC stack.
    PUSH(CDR(x));

    // Evaluate statement.
    PUSH_TAG(TAG_CONTINUE_BODY);
    x = CAR(x);
    goto do_eval;
next_body_statement:
    POP(x);
    goto do_body;

body_done:
    // Restore argument symbol values.
    while (POP_TAG(c) == TAG_ARG) {
        POP(name);
        POP(SYMBOL_VALUE(name));
    }

#ifndef NDEBUG
    if (c != TAG_DONE) {
        errouts ("Internal error: ");
        out_number (c);
        outs ("TAG_DONE expected.");
        while (1);
    }
#endif // #ifndef NDEBUG

    // Dispatch value based on tag.
got_value:
    if (value == delayed_eval)
        goto do_eval;
    POP_TAG(c);
    if (c != TAG_DONE) {
        switch (c) {
        case TAG_ARG_NEXT:
            goto next_arg;
        case TAG_BARG_NEXT:
            goto next_builtin_arg;
        case TAG_CONTINUE_BODY:
            goto next_body_statement;
        case TAG_CONTINUE_BLOCK:
            goto next_block_statement;
        }
#ifndef NDEBUG
        errouts ("Internal error: ");
        out_number (c);
        outs (": Unknown eval tag.");
        while (1);
#endif // #ifndef NDEBUG
    }
    return value;
}

lispptr
eval ()
{
#ifndef NDEBUG
    lispptr r;
    char * old_tagstack = tagstack;
#endif
    unevaluated = false;
    PUSH_TAG(TAG_DONE);
#ifdef NDEBUG
    return eval0 ();
#else
    r = eval0 ();
    if (old_tagstack != tagstack) {
        setout (STDERR);
        outs ("Internal error: tag stack: ");
        out_number ((long) (old_tagstack - tagstack));
        outs ("bytes off origin."); terpri ();
        while (1);
    }
    return r;
#endif // #ifdef NDEBUG
}

lispptr
funcall ()
{
    unevaluated = true;
    // Tell to return from eval0().
    PUSH_TAG(TAG_DONE);
    return eval0 ();
}
