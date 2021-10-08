#include <cbm.h>

#include <cc65-charmap.h>
#include <libterm.h>

#include "linebuf.h"
#include "line.h"

extern char get_key (void);

#define TTY_ENTER           13
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
    term_put (TERM_ESCAPE);
    term_put (TERM_ENABLE_ATTR);
    term_put (TERM_ATTR_REVERSE);
    term_puts ("UltiVI v0.1\n\r");
    term_put (TERM_ESCAPE);
    term_put (TERM_DISABLE_ATTR);
    term_put (TERM_ATTR_REVERSE);

    linebuf_clear ();

    while (1) {
        while (!(key = term_get ()));

        switch (key) {
            case TTY_ENTER:
                line_commit ();
                goto next;

            case TTY_CURSOR_LEFT:
                line_move_left ();
                goto next;

            case TTY_CURSOR_RIGHT:
                line_move_right ();
                goto next;

            case TTY_BACKSPACE:
                line_delete_char ();
                goto next;
        }

        line_insert_char (key);

next:
        line_redraw ();
    }
}
