#include <string.h>
#include <stdlib.h>

#include <lib/ingle/cc65-charmap.h>
#include <lib/term/libterm.h>
#include <lib/lineedit/liblineedit.h>
#include <lib/text/linelist.h>

#include "screen.h"


char columns = 40;
char rows = 24;

unsigned char ypos = 0;

unsigned ystart;
char * status = "";


void
gotoxy (char x, char y)
{
    term_put (TERM_SET_CURSOR);
    term_put (x);
    term_put (y);
}

void
set_cursor (void)
{
    gotoxy (xpos, ypos);
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
    gotoxy (0, ypos);
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

    if (len > columns)
        len = columns;

    while (len--)
        term_put (*data++);
}

void
print_status ()
{
    disable_cursor ();
    gotoxy (0, rows - 1);
    term_puts (status);
    term_put (TERM_CLEAR_TO_EOL);
    set_cursor ();
    enable_cursor ();
}

void
screen_set_status (char * msg)
{
    status = msg;
    print_status ();
}

void
update_screen_offset ()
{
    if (ystart > linenr)
        ystart = linenr;
    else if ((ystart + rows - 2) < linenr)
        ystart = linenr - rows + 2;
}

void
screen_redraw ()
{
    line      * l;
    unsigned    y;

    update_screen_offset ();
    disable_cursor ();
    l = linelist_get (ystart);

    for (y = 0; y < rows - 1; y++) {
        gotoxy (0, y);

        if (l) {
            line_redraw (l);
            l = l->next;
        } else
            term_put ('~');

        term_put (TERM_CLEAR_TO_EOL);

        if (y < rows - 1) {
            term_put (TERM_CARRIAGE_RETURN);
            term_put (TERM_LINE_FEED);
        }
    }

    print_status ();
    ypos = linenr - ystart;
    set_cursor ();
    enable_cursor ();
}

void
screen_init ()
{
    ystart = 0;
}
