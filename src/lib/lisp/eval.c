#include <ingle/cc65-charmap.h>

#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include <lisp/liblisp.h>
#include <simpleio/libsimpleio.h>

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
char * tagstack_start;
char * tagstack_end;
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
char has_error;
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
#pragma zpsym ("tagstack_start")
#pragma zpsym ("tagstack_end")
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
#pragma zpsym ("has_error")
#pragma zpsym ("c")
#pragma zpsym ("badef")
#pragma zpsym ("na")
#pragma zpsym ("unevaluated")
#pragma bss-name (pop)
#endif

void
internal_error (char * msg)
{
    setout (STDERR);
    outs ("Ouch! ");
    outs (msg);
    terpri ();
    print (last_eval_expr);
    terpri ();
    exit (EXIT_FAILURE);
}

void stack_overflow ()
{
    internal_error ("Stack overflow");
}

void stack_underflow ()
{
    internal_error ("Stack underflow");
}

void tagstack_overflow ()
{
    internal_error ("Tag stack overflow");
}

void tagstack_underflow ()
{
    internal_error ("Tag stack underflow");
}

char *
typename (lispptr * x)
{
    if (CONSP(x))
        return "cons";
    if (SYMBOLP(x))
        return "symbol";
    if (BUILTINP(x))
        return "built-in";
    if (NUMBERP(x))
        return "number";
    return "unknown type";
}


void
err_type (char * type, lispptr x)
{
    char * p;
    p = strcpy (buffer, type);
    p = strcpy (p, " expected. Got ");
    p = strcpy (p, typename (x));
    error (ERROR_TYPE, buffer);
}

// Type-check object.
void
bi_tcheck (lispptr x, uchar type)
{
    (void) x, (void) type;

    switch (type) {
    case 'x': // anything
        break;

    case 'n': // number
        if (!NUMBERP(x))
            err_type ("Number", x);
        break;

    case 's': // symbol
        if (!SYMBOLP(x))
            err_type ("Symbol", x);
        break;

    case 'c': // cons
        if (!CONSP(x))
            err_type ("Cons", x);
        break;

    case 'l': // list (cons or nil)
        if (!LISTP(x))
            err_type ("List", x);
        break;

    case 'f': // function
        if (!LISTP(x) && !BUILTINP(x))
            err_type ("Function", x);
        break;

#ifndef NDEBUG
    default:
        setout (STDERR);
        out (type);
        outs ("': unknown typedef");
        while (1);
#endif
    }
}

// Evaluate list to list of return values.
// TODO: Inline into eval0.
lispptr
eval_list (void)
{
    if (has_error)
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
    return make_cons (va, tmp);
}

lispptr
eval0 (void)
{
do_eval:
#ifndef NDEBUGGER
    last_eval_expr = x;
#endif
#ifdef VERBOSE_EVAL
    outs ("-> "); print (x); terpri ();
#endif
#ifdef GC_STRESS
    PUSH(x);
    gc ();
    POP(x);
#endif

    if (!x) {
        value = nil;
        goto do_return_atom;
    }

    // Evaluate atom.
    if (ATOM(x)) {
        value = SYMBOLP(x) ? SYMBOL_VALUE(x) : x;
        goto do_return_atom;
    }

    // Expression. Get function from symbol.
    arg1 = CAR(x);
#ifndef NDEBUG
    PUSH(arg1);
#endif
    if (arg1 && SYMBOLP(arg1)) {
        unevaluated = SPECIALP(arg1);
        arg1 = SYMBOL_VALUE(arg1);
    }

// Do BLOCK.

    if (arg1 == block_sym) {
        if (!CONSP(CDR(x))) {
            error (ERROR_ARG_MISSING, "No name.");
            goto do_return;
        }
        x = CDR(x);
        arg1 = CAR(x);

        if (!SYMBOLP(arg1)) {
            error (ERROR_TYPE, "Name not a sym.");
            goto do_return;
        }

        // Memorize start of expressions.
        arg2c = CDR(x);

        value = nil;
        unevaluated = false;
block_statement:
        if (has_error)
            goto do_return;
        x = CDR(x);
        if (!x)
            goto do_return;
        PUSH(arg1);     // Block name
        PUSH(arg2c);    // Expressions
        PUSH(x);        // Current expression
        x = CAR(x);
        PUSH_TAG(TAG_NEXT_BLOCK_STATEMENT);
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
                error (ERROR_TAG_MISSING, "Tag not found.");
                goto do_return;
            }
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
            goto do_return;
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
                error (ERROR_TOO_MANY_ARGS,
                       "Too many args to builtin:");
                goto do_return;
            }

set_arg_values:
            if (na == 1)
                POP(arg1);
            else if (na == 2) {
                POP(arg2);
                POP(arg1);
            }
#ifdef DEBUG_EVAL
            else if (na > 2) {
                internal_error ("#bargs");
                exit (-1);
            }
#endif

            // And call the built-in...
            POP_TAGW(bfun);
            value = bfun->func ();
            goto do_return;
        }

        na++;

        if (c == '?') {     // Optional argument.
            c = *++badef;
            if (!args) {
                PUSH(nil);
                goto set_arg_values;
            }
        } else if (!args) { // Missing argument.
            error (ERROR_ARG_MISSING,
                   "Missing arg to builtin.");
            goto do_return;
        }
        if (c == '\'') {    // Unevaluated argument.
            c = *++badef;
            if (c == '+') { // (Rest of arguments.)
                PUSH(args);
                goto set_arg_values;
            } else
                value = CAR(args);
            goto save_arg_value;
        }
        if (c == '+') {     // Rest of arguments.
            PUSH(args);
            PUSH_TAG(na);
            x = args;
            value = eval_list ();
            POP_TAG(na);
            POP(args);

            PUSH(value);
            goto set_arg_values;
        }

        // Evaluate argument inline.
        PUSH(args);
        PUSH_TAGW(badef);
        PUSH_TAG(na);
        x = CAR(args);
        PUSH_TAG(TAG_NEXT_BUILTIN_ARG);
        goto do_eval;
        // Step to next argument.
next_builtin_arg:
        POP_TAG(na);
        POP_TAGW(badef);
        POP(args);

        // Save for set_arg_values.
save_arg_value:
        // Ensure the type is wanted.
        bi_tcheck (value, *badef);
#ifndef NDEBUGGER
        while (has_error) {
            PUSH(args);
            PUSH_TAGW(badef);
            PUSH_TAG(na);
            value = lisp_repl ();
            POP_TAG(na);
            POP_TAGW(badef);
            POP(args);
            bi_tcheck (value, *badef);
        }
#endif
        badef++;

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
        error (ERROR_NOT_FUNCTION, "Not a fun.");
        goto do_return;
    }

    // Init argument list evaluation.
    defs = FUNARGS(arg1);
    na = 0;

    // Evaluate arguments to user-defined function.
do_argument:
    // End of arguments.
    if (!args && !defs)
        goto start_body;
    if (args && !defs) {
        error (ERROR_TOO_MANY_ARGS, "Too many args");
        goto restore_arguments;
    } else if (!args && CONSP(defs)) {
        error (ERROR_ARG_MISSING, "Arg missing");
        goto restore_arguments;
    }

    na++;

    // Rest of argument list. (consing)
    if (ATOM(defs)) {
        // Save old symbol value for return.
        PUSH(SYMBOL_VALUE(defs));

        if (unevaluated)
            value = args;
        else {
            // Evaluate rest of arguments.
            PUSH_TAG(na);
            PUSH(defs);
            PUSH(arg1);
            x = args;
            value = eval_list ();
            POP(arg1);
            POP(defs);
            POP_TAG(na);
        }

        // Assign rest of arguments.
        SET_SYMBOL_VALUE(defs, value);
        goto start_body;
    }

    // Save old argument symbol value.
    PUSH(SYMBOL_VALUE(CAR(defs)));

    if (unevaluated)
        value = CAR(args);
    else {
        // Save evaluator state.
        PUSH_TAG(na);
        PUSH(arg1);
        PUSH(defs);
        PUSH(args);

        PUSH_TAG(TAG_NEXT_ARG);
        x = CAR(args);
        goto do_eval;
        // Step to next argument.
next_arg:
        // Restore state.
        POP(args);
        POP(defs);
        POP(arg1);
        POP_TAG(na);
    }

    // Replace argument symbol value with evaluated one.
    SET_SYMBOL_VALUE(CAR(defs), value);

    // Step to next argument.
    defs = CDR(defs);
    args = CDR(args);
    goto do_argument;

    // Evaluate body.
start_body:
    unevaluated = false;
    PUSH_TAG(na);
    PUSH(FUNARGS(arg1));
    x = FUNBODY(arg1);

    // Evaluate body statement.
do_body:
    // Break if out of statements or other reason.
    if (!x || has_error)
        goto restore_arguments;

    // Save rest of statements on the GC stack.
    PUSH(CDR(x));

    // Evaluate statement.
    x = CAR(x);
    PUSH_TAG(TAG_NEXT_BODY_STATEMENT);
    goto do_eval;
next_body_statement:
    POP(x);
    goto do_body;

restore_arguments:
    // Restore argument symbol values.
    POP(defs);
    POP_TAG(na);
    c = na;
    while (c--) {
        if (ATOM(defs)) {
            if (defs)
                SET_SYMBOL_VALUE(defs, ((lispptr *)stack)[c]);
            break;
        }
        SET_SYMBOL_VALUE(CAR(defs), ((lispptr *)stack)[c]);
        defs = CDR(defs);
    }
    stack += sizeof (lispptr) * na;

do_return:
#ifndef NDEBUG
    POP(tmp);
#endif
do_return_atom:
    unevaluated = false;

    if (value == delayed_eval)
        goto do_eval;

#ifdef VERBOSE_EVAL
    outs ("<- "); print (value); terpri ();
#endif

    // Dispatch value based on tag.
    POP_TAG(c);
    if (c != TAG_DONE) {
        switch (c) {
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
        setout (STDERR);
        outs ("Internal error: ");
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
        outs ("B off origin."); terpri ();
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
