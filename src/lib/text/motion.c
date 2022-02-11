#include <string.h>
#include <stdlib.h>

#include <lib/ingle/cc65-charmap.h>
#include <lib/lineedit/liblineedit.h>
#include <lib/term/libterm.h>

#include "line.h"
#include "motion.h"

void
adjust_xpos_to_line_length ()
{
    line_goto (linenr);

    if (xpos >= current_line->length)
        xpos = current_line->length ?
            current_line->length - 1 :
            0;
}

void
move_up ()
{
    if (linenr)
        --linenr;

    adjust_xpos_to_line_length ();
}

void
move_down ()
{
    linenr++;
    if (linenr >= num_lines)
        linenr = num_lines - 1;

    adjust_xpos_to_line_length ();
}

void
move_left ()
{
    if (xpos)
        xpos--;
}

void
move_right ()
{
    unsigned l = current_line->length;

    if (l && xpos < l - 1)
        xpos++;
}

void
move_line_start ()
{
    xpos = 0;
}

void
move_line_end ()
{
    xpos = current_line->length;
}

void
move_line_begin ()
{
    if (!current_line->length)
        return;

    xpos = 0;
    while (current_line->data[xpos] == ' ' && xpos != current_line->length - 1)
        move_right ();
}

void
move_line_last_char ()
{
    if (current_line->length)
        xpos = current_line->length - 1;
}

void
move_last_line ()
{
    linenr = num_lines - 1;
    line_goto (linenr);
}
