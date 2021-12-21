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

    print_status ("-- INSERT --");

    while (1) {
        while (!(key = term_get ()));

        switch (key) {
            case TTY_ENTER:
                cmd_open_below ();
                continue;

            case TTY_ESCAPE:
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
    screen_init ();
    linelist_init ();
    screen_redraw ();
    command_mode ();
}
