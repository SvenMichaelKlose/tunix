#include <string.h>
#include <stdlib.h>

#include <cc65-charmap.h>
#include <libterm.h>
#include <liblineedit.h>

#include "linelist.h"
#include "screen.h"


unsigned ystart;


void
set_cursor (void)
{
    term_put (TERM_SET_CURSOR);
    term_put (xpos);
    term_put (linenr - ystart);
}

void
disable_cursor ()
{
    term_put (TERM_ESCAPE);
    term_put (TERM_DISABLE_ATTR);
    term_put (TERM_ATTR_CURSOR);
}

void
enable_cursor ()
{
    term_put (TERM_ESCAPE);
    term_put (TERM_ENABLE_ATTR);
    term_put (TERM_ATTR_CURSOR);
}

void
print_linebuf ()
{
    linebuf[linebuf_length] = 0;
    term_puts (linebuf);
}

// Called by liblineedit.
void
linebuf_redraw ()
{
    disable_cursor ();
    term_put (TERM_SET_CURSOR);
    term_put (0);
    term_put (linenr - ystart);
    term_put (TERM_CARRIAGE_RETURN);

    print_linebuf ();

    term_put (TERM_CLEAR_TO_EOL);
    set_cursor ();
    enable_cursor ();
}

void
line_redraw (line * l)
{
    char  * data = l->data;
    char    len  = l->length;

    while (len--)
        term_put (*data++);
}

void
screen_redraw ()
{
    line      * l = linelist_get (ystart + linenr);
    unsigned    y;

    disable_cursor ();
    term_put (TERM_CLEAR_SCREEN);

    for (y = 0; y < 24; y++) {
        if (l) {
            line_redraw (l);
            l = l->next;
        } else
            term_put ('~');

        if (y < 23) {
            term_put (TERM_CARRIAGE_RETURN);
            term_put (TERM_LINE_FEED);
        }
    }

    set_cursor ();
    enable_cursor ();
}

void
print_status (char * msg)
{
    disable_cursor ();
    term_put (TERM_SET_CURSOR);
    term_put (0);
    term_put (23);
    term_puts (msg);
    term_put (TERM_CLEAR_TO_EOL);
    set_cursor ();
    enable_cursor ();
}

void
screen_init ()
{
    ystart = 0;
}
