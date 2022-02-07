#include <string.h>
#include <stdlib.h>

#include <lib/ingle/cc65-charmap.h>
#include <lib/term/libterm.h>
#include <lib/lineedit/liblineedit.h>
#include <lib/lineedit/linebuf.h>
#include <lib/text/linelist.h>
#include <lib/text/motion.h>

#include "commands.h"
#include "screen.h"
#include "keyboard.h"


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

void
cmd_delete_till_line_end ()
{
    current_line->length = xpos;
    if (xpos)
        xpos--;
}

void
cmd_delete_char ()
{
    linelist_line_to_buf ();
    linebuf_delete_char (xpos);
    buf_to_linelist ();
    adjust_xpos_to_line_length ();
}

void
cmd_replace_char ()
{
    char c = get_key ();

    if (c < 32) {
        term_put (TERM_BELL);
        keyboard_init ();
        return;
    }

    linelist_line_to_buf ();
    linebuf_replace_char (xpos, c);
    buf_to_linelist ();
}

void
cmd_join ()
{
    unsigned len = current_line->length;

    if (!current_line->next)
        return;

    linelist_join ();
    xpos = len;
}
