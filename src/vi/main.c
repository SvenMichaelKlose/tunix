#include <cbm.h>

#include <lib/ingle/cc65-charmap.h>
#include <lib/term/libterm.h>
#include <lib/lineedit/liblineedit.h>
#include <lib/text/linelist.h>
#include <lib/text/motion.h>

#include "commands.h"
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

    linelist_goto (linenr);
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
    move_left ();
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
            case TTY_CURSOR_LEFT:
                move_left ();
                break;

            case 'k':
            case TTY_CURSOR_UP:
                move_up ();
                break;

            case 'j':
            case TTY_CURSOR_DOWN:
                move_down ();
                break;

            case 'l':
            case TTY_CURSOR_RIGHT:
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
