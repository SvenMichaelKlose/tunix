#include <string.h>
#include <stdlib.h>

#include <cc65-charmap.h>
#include <libterm.h>
#include <liblineedit.h>

#include "linelist.h"
#include "screen.h"


void
screen_redraw ()
{
    line  * ls;
    char  y;

    disable_cursor ();

    term_put (TERM_CLEAR_SCREEN);
    for (y = 0; y < 24; y++) {
        if (y == linenr)
            print_linebuf ();
        else {
            if (ls = linelist_get ((unsigned) y))
                term_puts (&ls->data);
            else
                term_put (0x7e);
        }

        if (y < 23) {
            term_put (TERM_CARRIAGE_RETURN);
            term_put (TERM_LINE_FEED);
        }
    }

    set_cursor ();
    enable_cursor ();
}
