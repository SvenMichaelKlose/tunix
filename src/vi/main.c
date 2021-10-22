#include <cbm.h>

#include <cc65-charmap.h>
#include <libterm.h>
#include <liblineedit.h>

#include "line.h"

#define TTY_ENTER           10
#define TTY_CURSOR_UP       5
#define TTY_CURSOR_DOWN     20
#define TTY_CURSOR_LEFT     19
#define TTY_CURSOR_RIGHT    4
#define TTY_BACKSPACE       8

int
main ()
{
    char key;

    term_init ();
    line_init ();
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
