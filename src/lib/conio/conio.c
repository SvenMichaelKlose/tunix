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

    term_put (TERM_ESCAPE);
    term_put (do_enable ? TERM_ENABLE_ATTR : TERM_DISABLE_ATTR);
    term_put (TERM_ATTR_CURSOR);

    cursor_enabled = do_enable;
    return tmp;
}
