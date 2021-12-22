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

void
edit_mode ()
{
    char key;

    linelist_line_to_buf ();
    screen_set_status ("-- INSERT --");
    screen_redraw ();

    while (1) {
        switch (key = wait_for_key ()) {
            case TTY_ENTER:
                linelist_buf_to_line ();
                if (xpos == linebuf_length)
                    linelist_insert_after ();
                else
                    linelist_split ();
                move_down ();
                move_line_start ();
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

            case 'O':
                cmd_open_above ();
                edit_mode ();
                continue;

            case 'o':
                cmd_open_below ();
                edit_mode ();
                continue;

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
