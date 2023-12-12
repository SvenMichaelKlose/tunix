#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include <cbm.h>

#include <lib/ingle/cc65-charmap.h>
#include <lib/term/libterm.h>

char teststr[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-*/";

void
test01 (void)
{
    int i, j;

    for (i = 0; i < 24; i++)
        for (j = 0; j < 40; j++)
            term_put (teststr[(i + j) % 40]);
    (void) term_get ();
}

void
test02 (void)
{
    term_puts ("Attempting reset...");
    term_puts ("\x1b" "c ");
    term_puts ("Resetted.");
    (void) term_get ();
}

void
test03 (void)
{
    term_put (TERM_CLEAR_SCREEN);
    term_put (TERM_SET_CURSOR);
    term_put (2);
    term_put (2);
    term_puts ("Text at 2:2.");
    (void) term_get ();
}

void
test04 (void)
{
    term_put (TERM_CLEAR_SCREEN);
    term_puts ("\x1b[3;10H");
    term_puts ("Text at 3:10.");
    (void) term_get ();
}

void
main (void)
{
    term_init ();
    test01 ();
    test02 ();
    test03 ();
    test04 ();
}
