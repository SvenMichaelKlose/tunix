#include <string.h>
#include <stdlib.h>

#include <cc65-charmap.h>
#include <libterm.h>
#include <liblineedit.h>

#include "linelist.h"
#include "motion.h"


void
line_open_below ()
{
    line_clear ();

    if (move_down ())
        linelist_insert ();
    else {
        linelist_append ();
        move_down ();
    }

    term_put (TERM_INSERT_LINE);
}
