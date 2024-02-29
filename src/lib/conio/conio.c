#include <stdbool.h>
#include <conio.h>

#include <lib/term/libterm.h>

void
gotoxy (unsigned char x, unsigned char y)
{
    term_put (TERM_SET_CURSOR);
    term_put (x);
    term_put (y);
}

bool cursor_enabled = true;

bool
cursor (bool do_enable)
{
    bool tmp = cursor_enabled;

    term_puts (do_enable ? "\x1b[?25h" : "\x1b[?25l");

    cursor_enabled = do_enable;
    return tmp;
}
