#include <string.h>
#include <stdlib.h>

#include <ingle/cc65-charmap.h>
#include <term/libterm.h>

#include "linebuf.h"
#include "liblineedit.h"

unsigned xpos;

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


/////////////
// EDITING //
/////////////

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

    linebuf_delete_char (xpos - 1);
    line_move_left ();
}


///////////////
// TOP LEVEL //
///////////////

void
lineedit (char key)
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
    linebuf_redraw ();
}

void
lineedit_init ()
{
    linebuf_clear ();
    xpos = 0;
}
