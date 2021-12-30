#include <string.h>
#include <stdlib.h>

#include <cc65-charmap.h>
#include <liblineedit.h>

#include "linelist.h"
#include "motion.h"

void
adjust_xpos_to_line_length ()
{
    linelist_goto (linenr);

    if (xpos > current_line->length)
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
    if (xpos < current_line->length)
        xpos++;
}

void
move_line_start ()
{
    xpos = 0;
}
