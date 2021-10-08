#include <cbm.h>

#include <cc65-charmap.h>
#include <libterm.h>

#include "linebuf.h"
#include "line.h"

extern char get_key (void);

int
main ()
{
    char key;

    term_init ();
    term_puts ("UltiVI v0.1\n\r");

    linebuf_clear ();

    while (1) {
        while (!(key = get_key ()));

        if (key == 13) {
            term_puts ("\n\r");
            line_clear ();
            continue;
        }

        line_insert_char (key);
        line_redraw ();
    }
}
