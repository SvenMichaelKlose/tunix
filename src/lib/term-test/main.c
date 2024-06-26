#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include <cbm.h>

#include <ingle/cc65-charmap.h>
#include <term/libterm.h>

char teststr[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-*/";

void
test_two_lines (void)
{
    term_puts ("Line 1.\n\r");
    term_puts ("Line 2.\n\r");
    (void) term_get ();
}

void
test_fill_with_chars (void)
{
    int i, j;

    term_put (TERM_CLEAR_SCREEN);
    term_puts ("\x1b[?25l");
    for (i = 0; i < 24; i++)
        for (j = 0; j < 40; j++)
            term_put (teststr[j]);
    for (i = 0; i < 24; i++)
        for (j = 0; j < 40; j++)
            term_put (teststr[j]);
    term_puts ("\x1b[?25h");
    (void) term_get ();
}

void
test_goto_all_lines (void)
{
    int y;

    term_put (TERM_CLEAR_SCREEN);
    for (y = 0; y < 24; y++) {
        term_put (TERM_SET_CURSOR);
        term_put (1);
        term_put (y + 1);
        term_puts ("Cursor motion to all lines.");
    }
    (void) term_get ();
}

void
test_scroll_up (void)
{
    int y;

    term_put (TERM_CLEAR_SCREEN);
    for (y = 0; y < 48; y++)
        term_puts ("This is supposed to scroll flawlessly.\n\r");
    (void) term_get ();
}

void
test_reset (void)
{
    term_puts ("Attempting reset...");
    term_puts ("\x1b" "c");
    term_puts ("Resetted.");
    (void) term_get ();
}

void
test_moved_cursor (void)
{
    term_put (TERM_CLEAR_SCREEN);
    term_put (TERM_SET_CURSOR);
    term_put (2);
    term_put (2);
    term_puts ("Text at 2:2.");
    (void) term_get ();
}

void
test_ansi_cursor_motion (void)
{
    term_put (TERM_CLEAR_SCREEN);
    term_puts ("\x1b[3;10H");
    term_puts ("Text at 3:10.");
    (void) term_get ();
}

void
test_ansi_vertical_moves (void)
{
    term_put (TERM_CLEAR_SCREEN);
    term_puts ("\x1b" "D");
    term_puts ("\x1b" "D");
    term_puts ("\x1b" "D");
    term_puts ("\x1b" "D");
    term_puts ("\x1b" "D");
    term_puts ("Text 5 lines down.");
    term_puts ("\x1b" "M");
    term_puts ("\x1b" "M");
    term_puts ("\x1b" "E");
    term_puts ("Text 3 lines down.");
    (void) term_get ();
}

void
doof (void)
{
}

void
main (void)
{
    term_init ();
    doof ();
    test_two_lines ();
    test_fill_with_chars ();
    test_goto_all_lines ();
    test_scroll_up ();
    test_reset ();
    test_moved_cursor ();
    test_ansi_cursor_motion ();
    test_ansi_vertical_moves ();

    term_put (TERM_CLEAR_SCREEN);
    term_puts ("Tests done.");
    while (1);
}
