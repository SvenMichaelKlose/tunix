#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#include <cbm.h>
#endif

#include <ctype.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#ifndef __CC65__
#include <signal.h>
#endif

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

char    load_fn = 12;
#ifndef NO_ONERROR
lispptr onerror_sym;
#endif

#ifdef __CC65__
#pragma bss-name (push, "ZEROPAGE")
#endif
extern lispptr x;
#ifdef __CC65__
#pragma zpsym ("x")
#pragma bss-name (pop)
#endif

char    num_repls;        // Number of REPLs - 1.
bool    do_break_repl;    // Tells current REPL to return.
bool    do_continue_repl; // If do_break_repl, tell REPL to continue.
bool    do_exit_program;  // Return to top-level REPL.
char    last_cmd;         // Last debugger short command.

lispptr FASTCALL
lisp_repl (char mode)
{
#ifndef NO_DEBUGGER
    char cmd;
#endif
#ifndef NDEBUG
    char * old_stack = stack;
    char * old_tagstack = tagstack;
#endif
    simpleio_chn_t this_in;
    simpleio_chn_t this_out;

    // Update and save I/O channels.
    if (mode != REPL_LOAD)
        set_channels (STDIN, STDOUT);
    this_in  = fnin;
    this_out = fnout;

#ifndef NO_DEBUGGER
    PUSH(current_toplevel);
#endif

    num_repls++;

    // Call error handler if defined.
#ifndef NAIVE
    if (has_error) {
#ifdef NO_DEBUGGER
        print_code_position ();
        exit (has_error);
#endif

#ifndef NO_ONERROR
        if (CONSP(SYMBOL_VALUE(onerror_sym))) {
            x = make_cons (onerror_sym, make_cons (make_number ((lispnum_t) has_error), make_cons (current_toplevel, make_cons (current_expr, nil))));
            has_error = false;
            unevaluated = true;
            PUSH_TAG(TAG_DONE);
            x = eval0 ();
            goto do_return;
        }
#endif // #ifndef NO_ONERROR
    }
#endif // #ifndef NAIVE

    // Read expresions from standard input until end
    // or until QUIT has been invoked.
    while (!eof ()) {
        if (mode != REPL_LOAD) {
#ifndef NO_DEBUGGER
            debug_step = nil;
            if (mode == REPL_DEBUGGER)
                print_code_position ();
#endif

            has_error = false;

            // Print prompt with number of recursions.
            if (num_repls)
                out ('0' + num_repls);
            outs ("* ");
        }

#ifndef NO_DEBUGGER
        // Read an expression.
        if (mode != REPL_DEBUGGER) {
#endif
            x = read ();
            if (mode != REPL_LOAD)
                fresh_line ();
#ifndef NO_DEBUGGER
        } else {
            cmd = 0;
            // Repeat last short command on ENTER.
            if (in () == 10) {
                cmd = last_cmd;
                fresh_line ();
            } else {
                // Read expression, get short command char.
                putback ();
                x = read ();
                fresh_line ();
                if (SYMBOLP(x) && SYMBOL_LENGTH(x) == 1)
                    cmd = SYMBOL_NAME(x)[0];
            }

            // Process short commands.
            if (cmd == 'c') { // Contine
                // Do not re-invoke debugger.
                debug_step = nil;
                goto do_return;
            } else if (cmd == 's') { // Step
                // Re-invoke before evaluating the next expression.
                debug_step = t; // T for any expression.
                last_cmd = cmd;
                outs ("Step..."); terpri ();
                goto do_return;
            } else if (cmd == 'n') { // Next
                // Re-invoke when done evaluating the current expression.
                debug_step = current_expr;
                last_cmd = cmd;
                outs ("Next..."); terpri ();
                goto do_return;
            }
        }
#endif

        // Save and update top-level expression.
#ifndef NO_DEBUGGER
        current_toplevel = x;
#endif

        // Evaluate expression.
        x = eval ();

        // Call debugger on error.
#ifndef NAIVE
        if (has_error)
            x = lisp_repl (REPL_DEBUGGER);
#endif

        // Break or continue on demand.
        if (do_break_repl) {
            // Ignore evaluation and contine with next expression.
            if (do_continue_repl) {
                do_break_repl = do_continue_repl = false;
                goto next;
            }
            if (do_exit_program) {
                // Return from all child REPLs.
                if (num_repls)
                    break;
            } else {
                // Just break this REPL.
                do_break_repl = false;
                break;
            }
            do_break_repl   = false;
            do_exit_program = false;
            setout (STDOUT); outs ("Program exited."); terpri ();
        }

        // Print result.
        if (mode != REPL_LOAD) {
            setout (STDOUT);
            print (x);
            fresh_line ();
        }

next:   set_channels (this_in, this_out);
    }

#ifndef NO_DEBUGGER
do_return:
#endif
    num_repls--;

    // Restore parent REPL's top-level expression.
#ifndef NO_DEBUGGER
    POP(current_toplevel);
#endif

#ifndef NDEBUG
    check_stacks (old_stack, old_tagstack);
#endif

    return x;
}

void FASTCALL
load (char * pathname)
{
    int oldin = fnin;

    simpleio_open (load_fn, pathname, 'r');
    arg1 = make_number (load_fn);
    bi_setin ();
#ifndef NAIVE
    if (err ()) {
        error (ERROR_FILE, pathname);
        goto err_open;
    }
#endif

    load_fn++;
    lisp_repl (REPL_LOAD);
    load_fn--;

    simpleio_close (load_fn);
#ifndef NAIVE
err_open:
#endif
    arg1 = make_number (oldin);
    bi_setin ();
}

void
init_repl ()
{
    do_break_repl = do_continue_repl = false;
    num_repls = -1;
}
