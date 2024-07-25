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

#ifndef NAIVE
lispptr current_toplevel;
#endif
char    num_repls;          // REPL count.
#ifndef NO_DEBUGGER
char    num_debugger_repls; // Debugger REPL count.
#endif
bool    do_break_repl;      // Tells current REPL to return.
bool    do_continue_repl;   // If do_break_repl, tell REPL to continue.
bool    do_exit_program;    // Return to top-level REPL.
#ifndef NO_DEBUGGER
char    last_cmd;           // Last debugger short command.
lispptr dbgx;               // Input debugger command.
#endif

lispptr FASTCALL
lisp_repl (char mode)
{
#ifndef NO_DEBUGGER
    char cmd;
#endif
#ifndef NDEBUG
    char * old_stack    = stack;
    char * old_tagstack = tagstack;
#endif
    simpleio_chn_t this_in;
    simpleio_chn_t this_out;

    // Update and save I/O channels.
    if (mode != REPL_LOAD)
        set_channels (STDIN, STDOUT);
    this_in  = fnin;
    this_out = fnout;

    num_repls++;
#ifndef NO_DEBUGGER
    if (mode == REPL_DEBUGGER)
        num_debugger_repls++;
#endif

#ifndef NAIVE
    // Handle error in other ways than calling the debugger.
    if (error_code) {
#ifndef NO_ONERROR
        // Call user-defined ONERROR handler.
        if (CONSP(SYMBOL_VALUE(onerror_sym))) {
            // Make argument list.
            x = make_cons (current_expr, nil);
            tmp = make_cons (current_toplevel, x);
            PUSH(tmp);
            x = make_number ((lispnum_t) error_code);
            POP(tmp);
            x = make_cons (x, tmp);
            x = make_cons (onerror_sym, x);

            // Call ONERROR.
            error_code  = 0;
            unevaluated = true;
            PUSH_TAG(TAG_DONE);
            x = eval0 ();
            goto do_return;
        }

#ifdef NO_DEBUGGER
        // Error not handled.  Exit program.
        print_error_info ();
        do_break_repl   = true;
        do_exit_program = true;
        error_code      = 0;
        goto do_return;
#endif
#endif // #ifndef NO_ONERROR

    }
#endif // #ifndef NAIVE

    // Read expresions from standard input until
    // end or until QUIT has been invoked.
    while (!eof ()) {
        if (mode != REPL_LOAD) {
#ifndef NO_DEBUGGER
            debug_step = nil;
            if (mode == REPL_DEBUGGER) {
                print_error_info ();
                error_code = 0;
            }
#endif

#ifndef NO_DEBUGGER
            // Print nesting count of debugger REPLs.
            if (num_debugger_repls)
                out ('0' + num_debugger_repls);
#endif
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
                dbgx = read ();
                fresh_line ();
                if (dbgx && SYMBOLP(dbgx) && SYMBOL_LENGTH(dbgx) == 1)
                    cmd = SYMBOL_NAME(dbgx)[0];
            }

            // Process short command.
            switch (cmd) {
                // Continue execution.
                case 'c':
                    debug_step = nil;
                    goto do_return;

                // Break on next eval0().
                case 's':
                    // Re-invoke before evaluating the next expression.
                    debug_step = t;
                    last_cmd = cmd;
                    goto do_return;

                // Break after current expression.
                case 'n':
                    debug_step = current_expr;
                    last_cmd = cmd;
                    goto do_return;

                // Print expression.
                case 'p':
                    outs ("?: ");
                    PUSH(x);
                    x = read ();
                    terpri ();
                    PUSH_TAG(TAG_DONE);
                    tmp = eval0 ();
                    POP(x);
                    outs ("Value: ");
                    print (tmp);
                    terpri ();
                    goto next;

                default:
                    x = dbgx;
            }
        }
#endif

#ifndef NAIVE
        // Memorize new top-level expression.
        current_toplevel = x;
        PUSH(current_toplevel);
#endif

        // Evaluate expression.
        x = eval ();

#ifndef NAIVE
        // Call debugger on error.
        if (error_code) {
            current_toplevel = *(lispptr *) stack;
            x = lisp_repl (REPL_DEBUGGER);
        }

        // Forget our expression.
        stack += sizeof (lispptr);
        current_toplevel = (lispptr) -1;
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

            // Break this REPL.
            do_break_repl = false;
            break;
        }

        // Print result.
        if (mode != REPL_LOAD) {
            setout (STDOUT);
            print (x);
            fresh_line ();
        }

next:
        do_break_repl    = false;
        do_continue_repl = false;

        // Restore I/O channels of this REPL.
        set_channels (this_in, this_out);
    }

#if !defined(NO_DEBUGGER) || !defined(NO_ONERROR)
do_return:
#endif

    // Track unnesting of this REPL.
    num_repls--;

#ifndef NO_DEBUGGER
    if (mode == REPL_DEBUGGER)
        num_debugger_repls--;
#endif

#ifndef NDEBUG
    check_stacks (old_stack, old_tagstack);
#endif

    return x;
}

void FASTCALL
load (char * pathname)
{
    // Memorize input channel.
    int oldin = fnin;

    // Open file.
    simpleio_open (load_fn, pathname, 'r');

    // Switch input channel to file.
    arg1 = make_number (load_fn);
    bi_setin ();

#ifndef NAIVE
    // Handle file error.
    if (err ()) {
        error (ERROR_FILE, pathname);
        goto err_open;
    }
#endif

    // Bump up file number for next LOAD.
    load_fn++;

    // Read file.
    lisp_repl (REPL_LOAD);

    // Back to former file number for next LOAD.
    load_fn--;

    // Close file.
    simpleio_close (load_fn);

#ifndef NAIVE
err_open:
#endif

    // Restore former input channel.
    arg1 = make_number (oldin);
    bi_setin ();
}

void
init_repl ()
{
    do_break_repl      = false;
    do_continue_repl   = false;
    num_repls          = 0;
#ifndef NO_DEBUGGER
    num_debugger_repls = 0;
#endif
    current_toplevel = nil;
}
