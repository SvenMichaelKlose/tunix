#include <string.h>
#include <stdlib.h>

#include <cc65-charmap.h>
#include <libterm.h>

#include "linebuf.h"
#include "liblineedit.h"

#define FALSE   0
#define TRUE    1

#define TTY_ENTER           13
#define TTY_CURSOR_UP       5
#define TTY_CURSOR_DOWN     20
#define TTY_CURSOR_LEFT     19
#define TTY_CURSOR_RIGHT    4
#define TTY_BACKSPACE       8


pos_t       xpos;
pos_t       ypos = 0;


void
error (char * txt)
{
    term_put (TERM_SET_CURSOR);
    term_put (0);
    term_put (23);
    term_puts (txt);
    while (1);
}


//////////////
// TERMINAL //
//////////////

void
set_cursor (void)
{
    term_put (TERM_SET_CURSOR);
    term_put (xpos);
    term_put (ypos);
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

void
line_redraw ()
{
    disable_cursor ();
    set_cursor ();
    term_put (TERM_CARRIAGE_RETURN);

    print_linebuf ();

    term_put (TERM_CLEAR_TO_EOL);
    set_cursor ();
    enable_cursor ();
}


////////////
// MOTION //
////////////

void
line_move_left ()
{
    if (xpos)
        xpos--;
}

void
line_move_right ()
{
    if (xpos < linebuf_length)
        xpos++;
}


//////////////////
// LINE EDITING //
//////////////////

void
line_clear ()
{
    linebuf_clear ();
    xpos = 0;
}

void
line_insert_char (char c)
{
    linebuf_insert_char (xpos, c);
    line_move_right ();
}

void
line_delete_char ()
{
    if (!xpos)
        return;

    linebuf_delete_char (xpos);
    line_move_left ();
}

void
line_edit (char key)
{
    switch (key) {
        case TTY_CURSOR_LEFT:
            line_move_left ();
            goto next;

        case TTY_CURSOR_RIGHT:
            line_move_right ();
            goto next;

        case TTY_BACKSPACE:
            line_delete_char ();
            goto next;
    }

    line_insert_char (key);

next:
    line_redraw ();
}
