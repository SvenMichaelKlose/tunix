#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <cbm.h>

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
wait4user (void)
{
    term_puts ("\n\rPress any key to continue...");
    get_key ();
}

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
cmd_change_till_line_end ()
{
    cmd_delete_till_line_end ();
    move_line_end ();
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

void
cmd_write_file ()
{
    line * l = first_line;
    char err;

    linebuf[linebuf_length] = 0;

    if (err = cbm_open (1, 8, 1, &linebuf[2])) {
        gotoxy (0, 23);
        term_puts ("Cannot open file '");
        term_puts (&linebuf[2]);
        term_puts ("':\n\r");
        term_puts (strerror (errno));
        wait4user ();
        return;
    }

    while (l) {
        cbm_write (1, l->data, l->length);
        cbm_write (1, "\n", 1);
        l = l->next;
    }

    cbm_close (1);

    gotoxy (0, 23);
    term_puts ("Wrote file '");
    term_puts (&linebuf[2]);
    term_puts ("'.");
    wait4user ();
}
