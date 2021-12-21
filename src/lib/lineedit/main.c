#include <string.h>
#include <stdlib.h>

#include <cc65-charmap.h>
#include <libterm.h>

#include "linebuf.h"
#include "liblineedit.h"


#define FALSE   0
#define TRUE    1


pos_t xpos;
pos_t ypos = 0;


void
error (char * txt)
{
    term_put (TERM_SET_CURSOR);
    term_put (0);
    term_put (23);
    term_puts (txt);
    while (1);
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
            goto done;

        case TTY_CURSOR_RIGHT:
            line_move_right ();
            goto done;

        case TTY_BACKSPACE:
            line_delete_char ();
            goto done;
    }

    line_insert_char (key);

done:
    line_redraw ();
}
