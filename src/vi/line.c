#include <string.h>

#include <libterm.h>

#include "linebuf.h"
#include "line.h"


pos_t       xpos;
pos_t       ypos = 0;
line        * first_line;


void
line_clear ()
{
    linebuf_clear ();
    xpos = 0;
}

void
line_set_cursor (void)
{
    term_put (TERM_SET_CURSOR);
    term_put (xpos);
    term_put (ypos);
}

void
line_commit ()
{
    line_clear ();
    if (ypos != 23)
        ypos++;

    term_put (TERM_LINE_FEED);
    term_put (TERM_INSERT_LINE);
}


void
line_redraw ()
{
    pos_t i;
    char c;

    line_set_cursor ();
    term_put (TERM_CARRIAGE_RETURN);

    for (i = 0; i < linebuf_length; i++) {
        if (!(c = linebuf[i]))
            return;

        term_put (c);
    }

    term_put (TERM_CLEAR_TO_EOL);
    line_set_cursor ();
}

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

line *
line_by_version (linestack * l, unsigned version)
{
    line  * m = &l->first;
    line  * n;

    if (m->version_deleted <= version || m->version > version)
        return NULL;

    while (n = m->newer) {
        if (n->version_deleted > version || n->version > version)
            break;

        m = n;
    }

    return m;
}

linestack *
linestack_get (unsigned i, unsigned version)
{
    linestack       * l = first_line;
    line   * m;

    do {
        m = line_by_version (l, version);
        if (m && !i--)
            return m;
    } while (l = l->next);

    return NULL;
}
