#include <string.h>
#include <stdlib.h>

#include <cc65-charmap.h>
#include <libterm.h>
#include <liblineedit.h>

#include "linelist.h"
#include "screen.h"

unsigned ystart;

void
screen_redraw ()
{
    line *    ls;
    unsigned  nr;
    unsigned  y;

    disable_cursor ();

    term_put (TERM_CLEAR_SCREEN);

    for (y = 0; y < 24; y++) {
        nr = ystart + y;

        if (nr == linenr)
            print_linebuf ();
        else {
            if (ls = linelist_get (nr))
                term_puts (&ls->data);
            else
                term_put ('~');
        }

        if (y < 23) {
            term_put (TERM_CARRIAGE_RETURN);
            term_put (TERM_LINE_FEED);
        }
    }

    set_cursor ();
    enable_cursor ();
}

void
screen_init ()
{
    ystart = 0;
}
