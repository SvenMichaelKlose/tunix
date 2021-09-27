#include <string.h>

#include <libterm.h>

#include "line.h"

char        line[MAX_LINE_LENGTH];
unsigned    line_length;
pos_t       xpos;

void
line_clear ()
{
    line[0] = 0;
    line_length = 0;
    xpos = 0;
}

void
line_set_cursor (void)
{
}

void
line_redraw_until_end (void)
{
    pos_t i;
    char c;

    term_put (TERM_CLEAR_TO_EOL);

    for (i = xpos; i < line_length; i++) {
        c = line[i];

        if (c < 32)
            return;

        term_put (c);
    }
}

void
line_move_left ()
{
    if (!xpos)
        return;

    xpos--;
    line_set_cursor ();
}

void
line_move_right ()
{
    if (xpos < line_length)
        xpos++;

    line_set_cursor ();
}

void
line_insert_char (char c)
{
    memmove (&line[xpos + 1], &line[xpos], line_length - xpos);
    line[xpos] = c;
    line_length++;
    line_redraw_until_end ();
    xpos++;
    line_set_cursor ();
}

void
line_delete_char ()
{
    if (!line_length || !xpos)
        return;

    xpos--;
    memmove (&line[xpos], &line[xpos + 1], line_length - xpos);
    line_length--;
    line_redraw_until_end ();
    line_set_cursor ();
}
