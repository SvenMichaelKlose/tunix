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
    term_put (7);
    term_put (26);
    term_puts ("fnord v0.1\n\r");
    term_puts ("1223334444");
    term_put (8);
    term_put (8);
    term_put (0x18);

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
