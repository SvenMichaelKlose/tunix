#include <string.h>
#include <stdlib.h>

#include <cc65-charmap.h>
#include <libterm.h>
#include <liblineedit.h>

#include "linelist.h"
#include "motion.h"

void
move_down ()
{
    linenr++;
    if (linenr >= num_lines)
        linenr = num_lines - 1;
}

void
move_up ()
{
    if (linenr)
        linenr--;
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
