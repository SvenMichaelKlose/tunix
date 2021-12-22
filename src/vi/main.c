#include <cbm.h>

#include <cc65-charmap.h>
#include <libterm.h>
#include <liblineedit.h>

#include "commands.h"
#include "linelist.h"
#include "screen.h"

void
edit_mode ()
{
    char key;

    linelist_line_to_buf ();
    print_status ("-- INSERT --");

    while (1) {
        while (!(key = term_get ()));

        switch (key) {
            case TTY_ENTER:
                linelist_buf_to_line ();
                cmd_open_below ();
                screen_redraw ();
                linelist_line_to_buf ();
                continue;

            case TTY_ESCAPE:
                linelist_buf_to_line ();
                print_status ("");
                return;

            default:
                lineedit (key);
        }
    }
}

void
command_mode ()
{
    char key;

    while (1) {
        while (!(key = term_get ()));

        switch (key) {
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
