#include <string.h>
#include <stdlib.h>

#include <cc65-charmap.h>
#include <libterm.h>
#include <liblineedit.h>

#include "linelist.h"
#include "motion.h"
#include "screen.h"


void
cmd_open_below ()
{
    linelist_insert_after ();
    move_down ();
}
