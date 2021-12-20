#include <string.h>
#include <stdlib.h>

#include <cc65-charmap.h>
#include <libterm.h>
#include <liblineedit.h>

#include "linelist.h"
#include "motion.h"
#include "screen.h"


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

    screen_redraw (); //term_put (TERM_INSERT_LINE);
}
