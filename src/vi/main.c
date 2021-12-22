#include <cbm.h>

#include <cc65-charmap.h>
#include <libterm.h>
#include <liblineedit.h>

#include "commands.h"
#include "linelist.h"
#include "screen.h"

char
wait_for_key ()
{
    char key;

    while (!(key = term_get ()));

    return key;
}

void
edit_mode ()
{
    char key;

    linelist_line_to_buf ();
    screen_set_status ("-- INSERT --");

    while (1) {
        switch (key = wait_for_key ()) {
            case TTY_ENTER:
                linelist_buf_to_line ();
                cmd_open_below ();
                linelist_line_to_buf ();
                screen_redraw ();
                continue;

            case TTY_ESCAPE:
                linelist_buf_to_line ();
                screen_set_status ("");
                return;

            default:
                lineedit (key);
        }
    }
}

void
command_mode ()
{
    while (1) {
        switch (wait_for_key ()) {
            case 'i':
                edit_mode ();
                continue;
        }
    }
}


int
main ()
{
    term_init ();
    linelist_init ();
    screen_init ();
    screen_redraw ();
    command_mode ();
}
