#include <string.h>
#include <stdlib.h>

#include <cc65-charmap.h>
#include <libterm.h>
#include <liblineedit.h>

#include "linelist.h"
#include "motion.h"
#include "screen.h"


void
cmd_open_above ()
{
    linelist_insert_before ();
    move_line_start ();
}

void
cmd_open_below ()
{
    linelist_insert_after ();
    move_down ();
    move_line_start ();
}

void
cmd_enter ()
{
    if (xpos == current_line->length)
        linelist_insert_after ();
    else
        linelist_split ();
    move_down ();
    move_line_start ();
}
