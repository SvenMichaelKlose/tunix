#ifdef __CC65__
#include <ingle/cc65-charmap.h>
#endif

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>

#include <simpleio/libsimpleio.h>
#include <lisp/liblisp.h>

lispptr breakpoints;

void
add_breakpoint (lispptr x)
{
    (void) x;
}

void
delete_breakpoint (lispptr x)
{
    (void) x;
}

void
list_breakpoints (void)
{
}

// Debugger REPL
//
// Called before (re)start of eval0().
void
debugger ()
{
    // Save old channels and switch to standard I/O.
    simpleio_chn_t oldin = fnin;
    simpleio_chn_t oldout = fnout;
    set_channels (STDIN, STDOUT);

    outs ("In debugger: "); terpri ();
    highlighting = current_expr;
    print (current_toplevel); terpri ();
    print (current_expr); terpri ();

    do_invoke_debugger = false;
    debug_step = nil;
    while (!eof ()) {
        outs ("] ");
        while (!eof () && in () < ' ');
        if (eof ())
            break;
        out (last_in); terpri ();
        if (last_in == 's') {
            debug_step = t;
            break;
        } else if (last_in == 'n') {
            debug_step = current_expr;
            break;
        } else if (last_in == 'b') {
            if (tmp = read_symbol ())
                add_breakpoint (tmp);
            else
                list_breakpoints ();
        } else if (last_in == 'd') {
            delete_breakpoint (read_number ());
        } else if (last_in == 'q')
            break;
    }

    set_channels (oldin, oldout);
}
