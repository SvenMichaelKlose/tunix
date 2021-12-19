#include <cbm.h>

#include <cc65-charmap.h>
#include <libterm.h>
#include <liblineedit.h>

#include "commands.h"
#include "linelist.h"
#include "screen.h"


int
main ()
{
    char key;

    term_init ();
    linelist_init ();
    screen_redraw ();

    while (1) {
        while (!(key = term_get ()));
        switch (key) {
            case TTY_ENTER:
                line_open_below ();
                continue;

            default:
                line_edit (key);
        }
    }
}
