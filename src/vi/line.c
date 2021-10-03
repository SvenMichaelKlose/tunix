#include <string.h>

#include <libterm.h>

#include "linebuf.h"
#include "line.h"


pos_t       xpos;
pos_t       ypos;
line        * first_line;


void
line_set_cursor (void)
{
    term_put (TERM_ESC);
    term_put (TERM_SET_CURSOR);
    term_put (xpos);
    term_put (ypos);
}

void
line_redraw_until_end (void)
{
    pos_t i;
    char c;

    for (i = xpos; i < linebuf_length; i++) {
        if (!(c = linebuf[i]))
            return;

        term_put (c);
    }

    term_put (TERM_CLEAR_TO_EOL);
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

line *
line_data (linestack * l, unsigned version)
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
line_get (unsigned i, unsigned version)
{
    linestack       * l = first_line;
    line   * m;

    do {
        m = line_data (l, version);
        if (m && !i--)
            return m;
    } while (l = l->next);

    return NULL;
}
