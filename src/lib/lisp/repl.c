#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#include <cbm.h>
#pragma inline-stdfuncs (off)
#pragma allow-eager-inline (off)
#endif

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <setjmp.h>
#ifdef TARGET_UNIX
#include <signal.h>
#endif

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

#ifndef NO_ONERROR
lispptr onerror_sym;
lispptr fail_sym;
#endif
#ifndef NAIVE
jmp_buf * hard_repl_break;
#endif
#ifndef NO_MACROEXPAND
lispptr macroexpand_sym;
bool    is_macroexpansion;  // TODO: Remove.
lispptr unexpanded_toplevel;
#endif

#ifndef NAIVE
lispptr current_toplevel;
#endif
char    num_repls;          // REPL count.
#ifndef NO_DEBUGGER
char    num_debugger_repls; // Debugger REPL count.
lispptr repl_value;         // Old (erroraneous) 'value'.
#endif
char    do_break_repl;      // Flag to break REPL for some reason.

#ifdef __CC65__
#pragma code-name ("CODE_REPL")
#endif

void
read_safe (void)
{
#ifndef NAIVE
    PUSH_TAG(error_code);
    error_code = 0;
#endif
    x = nil;
    x = read_expr ();
#ifndef NAIVE
    if (error_code)
        x = lisp_repl (REPL_DEBUGGER, 0);
    POP_TAG(error_code);
#endif
}

#ifndef NAIVE

void
out_colon (void)
{
    outs (": ");
}

void
print_debug_info ()
{
    // Head with errror info.
    fresh_line ();
    if (error_code) {
        // Error code.
        outs ("Error #");
        outn (error_code);
        out_colon ();

        // Human-readable description.
        if (last_errstr)
            outs (last_errstr);

        // Informative expression, describing the error
        // further.
        if (NOT_NIL(error_info)) {
            outs (": ");
            print (error_info);
        }
    } else {
        outs ("Rvalue: ");
        print (value);
    }

    fresh_line ();
    outs (error_code ? "In" : "Next");
#ifndef NO_HIGHLIGHTING
    do_highlight = true;
#endif
    if (NOT_NIL(current_function)) {
        tmp2 = SYMBOL_VALUE(current_function);
        print (current_function); // (Name)
        out (' ');
        tmp = FUNARGS(tmp2);
        if (tmp)
            print (FUNARGS(tmp2));
        else
            outs ("()");
        out_colon ();
        fresh_line ();
        print (FUNBODY(tmp2));
    } else {
        out_colon ();
        print (current_toplevel);
    }
#ifndef NO_HIGHLIGHTING
    do_highlight = false;
#endif
    fresh_line ();
}

#endif // #ifndef NAIVE

#ifndef NO_DEBUGGER

void
read_cmd_arg (void)
{
    PUSH(SYMBOL_VALUE(repl_value));
    PUSH(value);
    if (in () < ' ')
        value = nil;
    else {
        putback ();
        read_safe ();
        PUSH(highlighted);
        eval ();
        POP(highlighted);
    }
    terpri ();
}

#endif // #ifndef NO_DEBUGGER

lispptr FASTCALL
lisp_repl (char mode, simpleio_chn_t load_fn)
{
#ifndef NDEBUG
    char * old_stack    = stack;
    char * old_tagstack = tagstack;
#endif
#ifndef NAIVE
    // Save parent REPLs return point for hard errors.
    jmp_buf * old_break = hard_repl_break;

    // Our return point and stack pointers for hard errors.
    jmp_buf   this_break;
    char *    saved_stack;
    char *    saved_tagstack;
#endif
#ifndef NO_DEBUGGER
    char cmd;
#endif

    // Make sure the user can communicate should anything
    // go wrong if not actually running the program.
    setin (STDIN);
    setout (STDOUT);

    num_repls++;

#ifndef NAIVE
    if (error_code) {
#ifndef NO_ONERROR
        // Call user-defined ONERROR handler.
        if (CONSP(SYMBOL_VALUE(onerror_sym))) {
            // Save error state for the debugger
            // if ONERROR handler fails.
            PUSH(error_info);
            PUSH_TAG(error_code);
            PUSH_TAG(unevaluated);

            // Make argument list for call of ONERROR.
            x = make_cons (current_expr, nil);
            tmp2 = make_cons (current_toplevel, x);
            PUSH(tmp2);
            x = make_number ((lispnum_t) error_code);
            POP(tmp2);
            x = make_cons (x, tmp2);
            x = make_cons (onerror_sym, x);

            // Clear error status.
            error_info  = nil;
            error_code  = 0;
            unevaluated = true;

            // Switch to program channels.
            setin (NUMBER_VALUE(SYMBOL_VALUE(lisp_fnin)));
            setout (NUMBER_VALUE(SYMBOL_VALUE(lisp_fnout)));

            // Call ONERROR.
            PUSH_TAG(TAG_DONE);
            x = eval0 ();

            // Back to standard I/O channels.
            setin (STDIN);
            setout (STDOUT);

            // Handle error as usual if %FAIL was returned.
            if (x != fail_sym) {
                // Discard saved context.
                stack += 1 * sizeof (lispptr);
                tagstack += 2;

                // Continue with new value.
                goto done_onerror;
            }

            // Restore context for debugger.
            POP_TAG(unevaluated);
            POP_TAG(error_code);
            POP(error_info);
        }
#ifdef NO_DEBUGGER
        // Error not handled.  Exit program.
        print_debug_info ();
        do_break_repl = BRK_EXIT;
        goto do_return;
#endif
#endif // #ifndef NO_ONERROR
    }

#endif // #ifndef NAIVE

    // READ/EVAL/PRINT-Loop.
    while (1) {
        setin (load_fn ? load_fn : NUMBER_VALUE(SYMBOL_VALUE(lisp_fnin)));
        if (eof ())
            break;
        setin (STDIN);

#ifndef NO_DEBUGGER
        if (mode == REPL_DEBUGGER) {
            // TODO: Save terminal flags.
            outs ("\002\004"); // Normal terminal mode.
            SET_SYMBOL_VALUE(repl_value, value);
            num_debugger_repls++;
            debug_step = nil;
            fresh_line ();
            outs ("DEBUGGER ");
            outn (num_debugger_repls);
            out (':');
            terpri ();
            print_debug_info ();
#ifdef EXIT_FAILURE_ON_ERROR
            exit (EXIT_FAILURE);
#endif
        }

        // Read an expression.
        if (mode != REPL_DEBUGGER) {
#endif // #ifndef NO_DEBUGGER
            setin (load_fn ? load_fn : NUMBER_VALUE(SYMBOL_VALUE(lisp_fnin)));
            read_safe ();
            if (eof ())
                break;
            setin (STDIN);
#ifdef VERBOSE_READ
            // TODO: Set/restore terminal mode.
            print (x);
#endif
#ifndef TARGET_UNIX
            if (mode != REPL_LOAD)
                terpri ();
#endif
#ifndef NO_DEBUGGER
        } else {
            // Read short debugger command, skipping whitespace.
            do {
                cmd = in ();
            } while (!eof () && cmd < ' ');
#ifdef TARGET_UNIX
            if (eof ())
                exit (EXIT_FAILURE);
#endif
            // Process short command.
            fresh_line ();
            switch (cmd) {
                // Continue execution.
                case 'c':
                    if (error_code)
                        goto cannot_continue;
                    break;

                // Step into function.
                case 's':
                    if (error_code)
                        goto cannot_continue;

                    // Break on next expression.
                    debug_step = t;
                    break;

                // Step over function call.
                case 'n':
                    if (error_code)
                        goto cannot_continue;

                    // Break *after* current expression.
                    debug_step = current_expr;
                    break;

                // Print expression without affecting the
                // return value.
                case 'p':
                    read_cmd_arg ();
                    outs ("Value: ");
                    print (value);
                    goto done_short_command;

                // Set breakpoint or print all of them.
                case 'b':
                    read_cmd_arg ();
                    if (_NAMEDP(value)) {
                        if (NOT_NIL(value))
                            SET_SYMBOL_VALUE(breakpoints_sym,
                                             make_cons (value, SYMBOL_VALUE(breakpoints_sym)));
                        goto print_breakpoints;
                    }
                    goto want_name;

                // Delete selected or all breakpoints.
                case 'd':
                    read_cmd_arg ();
                    if (SYMBOLP(value)) {
                        if (NOT_NIL(value))
                            copy_list (SYMBOL_VALUE(breakpoints_sym), COPY_REMOVE, value);
                        SET_SYMBOL_VALUE(breakpoints_sym, value);
                    }
                    goto print_breakpoints;

                // Skip to next expression.
                case 'k':
                    do_break_repl = BRK_CONTINUE;
                    goto break_repl;

                // Return from REPL.
                case 'q':
                    do_break_repl = BRK_RETURN;
                    goto break_repl;

                // Exit program.
                case 'x':
                    do_break_repl = BRK_EXIT;
break_repl:
                    error_code = 0;
                    error_info = nil;
                    break;
cannot_continue:
                    outs ("Need alternative first!");
                    goto terpri_next;
want_name:
                    outs ("Name!");
                    goto done_short_command;
print_breakpoints:
                    outs ("Breakpoints: ");
                    print (SYMBOL_VALUE(breakpoints_sym));
done_short_command:
                    POP(value);
                    POP(tmp);
                    SET_SYMBOL_VALUE(repl_value, tmp);
terpri_next:
                    terpri ();
                    break;

                default:
                    // It wasn't a debugger command.
                    // Read as expression to evaluate.
                    putback ();
                    read_safe ();
                    if (NOT(x))
                        break;
#ifndef TARGET_UNIX
                    terpri ();
#endif
            }
        }
#endif
#ifndef NO_MACROEXPAND
#ifndef NAIVE
        // Memorize unexpanded expression.
        PUSH(unexpanded_toplevel);
        unexpanded_toplevel = x;
        current_toplevel    = x;
#endif
#ifndef NO_DEBUGGER
        // Reset error status for next evaluation.
        error_code = 0;
        error_info = nil;
#endif
        // Macro expansion.
        if (CONSP(SYMBOL_VALUE(macroexpand_sym))) {
#ifndef NO_DEBUGGER
            // Avoid debug step into MACROEXPAND.
            PUSH(debug_step);
            debug_step = nil;
#endif
            // Make arguments list for MACROEXPAND.
            x = make_cons (x, nil);
            x = make_cons (macroexpand_sym, x);

            // Switch to program's U/O c
            setin (NUMBER_VALUE(SYMBOL_VALUE(lisp_fnin)));
            setout (NUMBER_VALUE(SYMBOL_VALUE(lisp_fnout)));

            // Call MACROEXPAND.
            unevaluated = true;
            PUSH_TAG(TAG_DONE);
            x = eval0 ();

            // Switch back to standard I/O.
            setin (STDIN);
            setout (STDOUT);

#ifndef NO_DEBUGGER
            POP(debug_step);
#endif
        }
#endif // #ifndef NO_MACROEXPAND

#ifndef NAIVE
        // Set as new top-level expression for REPL_DEBUGGER.
        PUSH(current_toplevel);
        current_toplevel = x;
#endif
#ifndef NAIVE
        // Init catching errors.
        if (!setjmp (this_break)) {
            // Save return point.
            hard_repl_break = &this_break;

            // Save GC and tag stack pointers.
            saved_stack     = stack;
            saved_tagstack  = tagstack;
#endif
            // Set program channels.
            setin (NUMBER_VALUE(SYMBOL_VALUE(lisp_fnin)));
            setout (NUMBER_VALUE(SYMBOL_VALUE(lisp_fnout)));

            // Evaluate expression.
            x = eval ();

            setin (STDIN);
            setin (STDOUT);
#ifndef NAIVE
            hard_repl_break = old_break;
        } else {
            // Restore GC and tag stack pointers.
            stack    = saved_stack;
            tagstack = saved_tagstack;

            // Restore parent REPLs return point.
            hard_repl_break = old_break;
#ifdef FRAGMENTED_HEAP
            // Ensure that the GC is not just switching to
            // the next heap.
            while (heap->start)
                switch_heap ();
#endif
            // Free what is now unused.
            x = nil;
            gc ();
        }
#endif // #ifndef NAIVE
#if !defined(NO_DEBUGGER) && !defined(NO_ONERROR)
        // Catch lost RETURN and GO.
        if (!error_code) {
            if (x == return_sym)
                error (ERROR_LOST_RETURN, "RETURN without BLOCK");
            else if (x == go_sym)
                error (ERROR_LOST_GO, "GO without BLOCK");
            if (error_code)
                x = lisp_repl (REPL_DEBUGGER, 0);
        }
#endif // #if !defined(NO_DEBUGGER) && !defined(NO_ONERROR)
#ifndef NAIVE
        POP(current_toplevel);
#ifndef NO_MACROEXPAND
        POP(unexpanded_toplevel);
#endif
#endif // #ifndef NAIVE

        // Break mode.
        if (do_break_repl == BRK_CONTINUE) {
            do_break_repl = 0;
            continue;
        } else if (do_break_repl == BRK_RETURN) {
            do_break_repl = 0;
            break;
        } else if (do_break_repl == BRK_EXIT) {
            // Return from child REPLs.
            if (num_repls)
                break;

            // Clear break mode.
            do_break_repl = 0;
            setout (STDOUT);
            outs ("Program exited."); terpri ();
            continue;
        }

        // Print result of user input.
        if (mode != REPL_LOAD) {
            setout (STDOUT);    // TODO: Clean up setting standard I/O.
            print (x);
            fresh_line ();
        }
    }
#if !defined(NO_ONERROR) && defined(NO_DEBUGGER)
do_return:
#endif
#if !defined(NO_ONERROR) || !defined(NO_DEBUGGER)
    // Track unnesting of this REPL.
    if (mode == REPL_DEBUGGER) {
#ifndef NO_DEBUGGER
        outs ("<debugger"); terpri ();
        num_debugger_repls--;
#endif

done_onerror:
        // Restore earlier GC trigger threshold to
        // be able to run ONERROR if out of heap.
        onetime_heap_margin = ONETIME_HEAP_MARGIN;
        // TODO: Restore terminal flags.
    }
#endif
    num_repls--;

#ifndef NDEBUG
    check_stacks (old_stack, old_tagstack);
#endif

    return x;
}

#ifndef NO_BUILTIN_GROUP_FILE

bool FASTCALL
load (char * pathname)
{
    char status = false;
    simpleio_chn_t load_fn;

#ifdef VERBOSE_LOAD
    outs ("Loading "); outs (pathname); terpri ();
#endif

    // Open file.
    strcpy (buffer, pathname);
    load_fn = simpleio_open (buffer, 'r');
    if (!load_fn)
        return false;

#ifndef NAIVE
    // Handle file error.
    if (err ())
        goto err_open;
#endif

    // Read file.
    lisp_repl (REPL_LOAD, load_fn);

    // Close file.
    simpleio_close (load_fn);
    status = true;

#ifndef NAIVE
err_open:
#endif
    return status;
}

#endif // #ifndef NO_BUILTIN_GROUP_FILE

#ifdef TARGET_VIC20
#pragma code-name ("CODE_INIT")
#endif

void
init_repl ()
{
    do_break_repl = 0;
    num_repls     = -1;
#ifndef NO_DEBUGGER
    num_debugger_repls = 0;
    repl_value = make_symbol ("*r*", 3);
    expand_universe (repl_value);
#endif
#ifndef NO_ONERROR
    onerror_sym = make_symbol ("onerror", 7);
    expand_universe (onerror_sym);
    fail_sym = make_symbol ("%fail", 5);
    expand_universe (fail_sym);
#endif
#ifndef NO_MACROEXPAND
    unexpanded_toplevel = nil;
#endif
#ifndef NAIVE
    current_toplevel = nil;
#endif
#ifndef NO_MACROEXPAND
    macroexpand_sym = make_symbol ("macroexpand", 11);
    expand_universe (macroexpand_sym);
#endif
}
