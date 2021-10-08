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
    term_puts ("UltiVI v0.1\n");

    linebuf_clear ();

    while (1) {
        while (!(key = get_key ()));
        term_put (key);
    }
}
