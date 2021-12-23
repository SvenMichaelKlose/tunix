#include <cbm.h>

#include <cc65-charmap.h>
#include <libterm.h>
#include <liblineedit.h>

#include "commands.h"
#include "linelist.h"
#include "motion.h"
#include "screen.h"

char
wait_for_key ()
{
    char key;

    while (!(key = term_get ()));

    return key;
}

char
lineedit_mode ()
{
    char key;

    linelist_line_to_buf ();

    while (1) {
        switch (key = wait_for_key ()) {
            case TTY_ENTER:
            case TTY_ESCAPE:
                goto done;

            default:
                lineedit (key);
        }
    }

done:
    linelist_buf_to_line ();

    return key;
}

void
edit_mode ()
{
    char key;

    screen_set_status ("-- INSERT --");

    while (1) {
        screen_redraw ();

        switch (key = lineedit_mode ()) {
            case TTY_ENTER:
                cmd_enter ();
                continue;

            case TTY_ESCAPE:
                goto done;
        }
    }

done:
    screen_set_status ("");
}

void
command_mode ()
{
    while (1) {
        linelist_goto (linenr);

        switch (wait_for_key ()) {
            case 'i':
                edit_mode ();
                break;

            case 'O':
                cmd_open_above ();
                edit_mode ();
                break;

            case 'o':
                cmd_open_below ();
                edit_mode ();
                break;

            case 'h':
                move_left ();
                break;

            case 'k':
                move_up ();
                break;

            case 'j':
                move_down ();
                break;

            case 'l':
                move_right ();
                break;
        }

        screen_redraw ();
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
