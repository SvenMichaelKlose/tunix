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
lispptr debugger_return_value_sym; // Old (erroraneous) 'value'.
#endif
bool    do_break_repl;      // Tells current REPL to return.
bool    do_continue_repl;   // If do_break_repl, tell REPL to continue.
bool    do_exit_program;    // Return to top-level REPL.

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
        x = lisp_repl (REPL_DEBUGGER);
    POP_TAG(error_code);
#endif
}

#ifndef NO_DEBUGGER

void
out_colon (void)
{
    outs (": ");
}

void
print_debugger_info ()
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

    // Print is either about to be evaluated or caused
    // an error.
    fresh_line ();
    outs (error_code ? "In" : "Next");
    do_highlight = true;
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
        terpri ();
        print (FUNBODY(tmp2));
    } else {
        out_colon ();
        print (current_toplevel);
    }
    do_highlight = false;
    terpri ();
}

void
read_cmd_arg (void)
{
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

simpleio_chn_t app_in;
simpleio_chn_t app_out;

lispptr FASTCALL
lisp_repl (char mode)
{
    simpleio_chn_t read_chn = (mode == REPL_LOAD) ? fnin : STDIN;
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

    set_channels (STDIN, STDOUT);

    num_repls++;
#ifndef NO_DEBUGGER
    // Tell about debugger and which one it is.
    if (mode == REPL_DEBUGGER) {
        SET_SYMBOL_VALUE(debugger_return_value_sym, value);
        num_debugger_repls++;
        fresh_line ();
        outs ("DEBUGGER ");
        outn (num_debugger_repls);
        out (':');
        terpri ();
    }
#endif
#ifndef NAIVE
    if (error_code) {
#ifndef NO_ONERROR
        // Call user-defined ONERROR handler.
        if (CONSP(SYMBOL_VALUE(onerror_sym))) {
            // Make argument list.
            x = make_cons (current_expr, nil);
            tmp2 = make_cons (current_toplevel, x);
            PUSH(tmp2);
            x = make_number ((lispnum_t) error_code);
            POP(tmp2);
            x = make_cons (x, tmp2);
            x = make_cons (onerror_sym, x);

            // Call ONERROR.
            error_code  = 0;
            error_info  = nil;
            unevaluated = true;
            PUSH_TAG(TAG_DONE);
            x = eval0 ();
            goto do_return;
        }
#ifdef NO_DEBUGGER
        // Error not handled.  Exit program.
        // TODO?: print_debugger_info ();
        do_break_repl   = true;
        do_exit_program = true;
        error_code      = 0;
        error_info      = nil;
        goto do_return;
#endif
#endif // #ifndef NO_ONERROR
    }
#endif // #ifndef NAIVE

    // READ/EVAL/PRINT-Loop.
    while (!eof ()) {
        setin (read_chn);
        if (eof ())
            break;
        setin (STDIN);

#ifndef NO_DEBUGGER
        if (mode == REPL_DEBUGGER) {
            print_debugger_info ();
#ifdef EXIT_FAILURE_ON_ERROR
        exit (EXIT_FAILURE);
#endif
            // Out of heap errors cannot be corrected.
            if (error_code == ERROR_OUT_OF_HEAP)
                error_code = 0;
        }
#endif
#ifndef NO_DEBUGGER
        // Read an expression.
        if (mode != REPL_DEBUGGER) {
#endif
            setin (read_chn);
            read_safe ();
            if (eof ())
                goto do_return;
#ifdef VERBOSE_READ
            PUSH_TAG(fnout);
            setout (STDOUT);
            print (x);
            setout (fnout);
            POP_TAG(fnout);
#endif
            setin (STDIN);
#ifndef TARGET_UNIX
            if (mode != REPL_LOAD)
                terpri ();
#endif
#ifndef NO_DEBUGGER
        } else {
            // Read short debugger command, skipping whitespaces.
            do {
                cmd = in ();
            } while (!eof () && cmd < ' ');
#ifdef TARGET_UNIX
            if (eof ())
                exit (EXIT_FAILURE);
#endif
            // Process short command.
            fresh_line ();
            debug_step = nil;
            switch (cmd) {
                // Continue execution.
                case 'c':
                    if (error_code)
                        goto cannot_continue;
                    goto do_return;

                // Step into function.
                case 's':
                    if (error_code)
                        goto cannot_continue;

                    // Break on next expression.
                    debug_step = t;
                    goto do_return;

                // Step over function call.
                case 'n':
                    if (error_code)
                        goto cannot_continue;

                    // Break *after* current expression.
                    debug_step = current_expr;
                    goto do_return;

                // Print expression without affecting the
                // return value.
                case 'p':
                    PUSH(SYMBOL_VALUE(debugger_return_value_sym));
                    PUSH(value);
                    read_cmd_arg ();
                    outs ("Value: ");
                    print (value);
                    goto done_short_command;

                // Set breakpoint or print all of them.
                case 'b':
                    PUSH(SYMBOL_VALUE(debugger_return_value_sym));
                    PUSH(value);
                    read_cmd_arg ();
                    if (SYMBOLP(value)) {
                        if (NOT_NIL(value))
                            SET_SYMBOL_VALUE(breakpoints_sym,
                                             make_cons (value, SYMBOL_VALUE(breakpoints_sym)));
                        goto print_breakpoints;
                    }
                    goto want_symbol;

                // Delete specific or all breakpoints.
                case 'd':
                    PUSH(SYMBOL_VALUE(debugger_return_value_sym));
                    PUSH(value);
                    read_cmd_arg ();
                    if (SYMBOLP(value)) {
                        if (NOT_NIL(value))
                            copy_list (SYMBOL_VALUE(breakpoints_sym), COPY_REMOVE, value);
                        SET_SYMBOL_VALUE(breakpoints_sym, value);
                    }
                    goto print_breakpoints;

                // Exit program
                case 'q':
                    arg1 = nil;
                    do_break_repl   = true;
                    do_exit_program = true;
                    error_code = 0;
                    goto do_return;
cannot_continue:
                    outs ("Need alternative first!");
                    goto terpri_next;
want_symbol:
                    outs ("Symbol!");
                    goto done_short_command;
print_breakpoints:
                    outs ("Breakpoints: ");
                    print (SYMBOL_VALUE(breakpoints_sym));
done_short_command:
                    POP(value);
                    POP(tmp);
                    SET_SYMBOL_VALUE(debugger_return_value_sym, tmp);
terpri_next:
                    terpri ();
                    goto next;

                default:
                    // It wasn't a debugger command.
                    // Read as expression to evaluate.
                    putback ();
                    read_safe ();
                    if (NOT(x))
                        goto next;
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
        current_toplevel = x;
#endif
#ifndef NO_DEBUGGER
        // Reset error status for next evaluation.
        error_code = 0;
        error_info = nil;
#endif
        // Macro expansion if MACROEXPAND is a user function.
        if (CONSP(SYMBOL_VALUE(macroexpand_sym))) {
#ifndef NO_DEBUGGER
            // Avoid debug step into MACROEXPAND.
            PUSH(debug_step);
            debug_step = nil;
#endif
            // Call MACROEXPAND.
            x = make_cons (x, nil);
            x = make_cons (macroexpand_sym, x);
            unevaluated = true;
            PUSH_TAG(TAG_DONE);
            x = eval0 ();
#ifndef NO_DEBUGGER
            POP(debug_step);
#endif
        }
#endif // #ifndef NO_MACROEXPAND
#ifndef NAIVE
        PUSH(current_toplevel);
        current_toplevel = x;
#endif
#ifndef NO_DEBUGGER
        highlighted = nil; // TODO: Explain.
#endif
#ifndef NAIVE
        if (!setjmp (this_break)) {
            // Save return point for hard errors, like out
            // of heap or internal errors.
            hard_repl_break = &this_break;

            // Save GC and tag stack pointers.
            saved_stack     = stack;
            saved_tagstack  = tagstack;
#endif
            // Set program channels.
            set_channels (app_in, app_out);

            // Evaluate expression.
            x = eval ();

            // Update saved program channels.
            app_in  = fnin;
            app_out = fnout;
            set_channels (STDIN, STDOUT);
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
#endif
#ifndef NAIVE
        // Call debugger on error.
        if (error_code)
            x = lisp_repl (REPL_DEBUGGER);

        POP(current_toplevel);
#ifndef NO_MACROEXPAND
        POP(unexpanded_toplevel);
#endif
#endif
        // Special treatment of results.
        if (do_break_repl) {
            // Ignore result and continue with next expression.
            if (do_continue_repl)
                goto next;

            // Exit program.
            if (do_exit_program) {
                // First return from all nested REPLs.
                if (num_repls)
                    break;

                // Print message.
                setout (STDOUT);
                outs ("Program exited."); terpri ();

                goto next;
            }

            // Break this REPL if it's not the topmost.
            do_break_repl = false;
            if (num_repls)
                break;
        }
#ifndef NO_DEBUGGER
        // Continue with alternative value.
        if (mode == REPL_DEBUGGER) {
            debug_step = t;
            goto do_return;
        }
#endif
        // Print result of user input.
        if (mode != REPL_LOAD) {
            setout (STDOUT);
            print (x);
            fresh_line ();
        }
next:
        do_break_repl    = false;
        do_continue_repl = false;
    }
do_return:
    // Track unnesting of this REPL.
    num_repls--;
#ifndef NO_DEBUGGER
    if (mode == REPL_DEBUGGER) {
        num_debugger_repls--;
        onetime_heap_margin = ONETIME_HEAP_MARGIN;

        outs ("Continuing...");
        terpri ();
    }
#endif
#ifndef NDEBUG
    check_stacks (old_stack, old_tagstack);
#endif

    // Restore program channels.
    set_channels (app_in, app_out);

    return x;
}

bool FASTCALL
load (char * pathname)
{
    char status = false;
    simpleio_chn_t load_fn;

    // Memorize input channel.
    int oldin = fnin;

#ifdef VERBOSE_LOAD
    outs ("Loading "); outs (pathname); terpri ();
#endif

    // Open file.
    strcpy (buffer, pathname);
    load_fn = simpleio_open (buffer, 'r');
    if (!load_fn)
        return false;

    // Switch input channel to file.
    arg1 = make_number (load_fn);
    bi_setin ();

#ifndef NAIVE
    // Handle file error.
    if (err ())
        goto err_open;
#endif

    // Read file.
    lisp_repl (REPL_LOAD);

    // Close file.
    simpleio_close (load_fn);
    status = true;

#ifndef NAIVE
err_open:
#endif
    // Restore former input channel.
    arg1 = make_number (oldin);
    bi_setin ();

    return status;
}

#ifdef TARGET_VIC20
#pragma code-name ("CODE_INIT")
#endif

void
init_repl ()
{
    do_break_repl      = false;
    do_continue_repl   = false;
    num_repls = -1;
    app_in = STDIN;
    app_out = STDOUT;
#ifndef NO_DEBUGGER
    num_debugger_repls = 0;
    debugger_return_value_sym = make_symbol ("*r*", 3);
    expand_universe (debugger_return_value_sym);
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
