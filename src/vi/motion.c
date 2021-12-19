#include <string.h>
#include <stdlib.h>

#include <cc65-charmap.h>
#include <libterm.h>
#include <liblineedit.h>

#include "linelist.h"
#include "motion.h"

char
move_down ()
{
    if (linenr == num_lines - 1)
        return FALSE;

    linelist_goto (++linenr);

    if (ypos != 23)
        ypos++;

    term_put (TERM_LINE_FEED);

    return TRUE;
}
