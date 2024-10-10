#include <ctype.h>
#include <stdbool.h>
#include <stddef.h>

#include <simpleio/libsimpleio.h>
#include <simpleio/control.h>

char 
con_reset (void)
{   
    char f;
    out (TERM_CMD_GET);
    f = conin ();
    if (f & TERM_FLAG_DIRECT)
        out (TERM_CMD_CLRSCR);
    out (TERM_CMD_CLR);
    out (255);
    out (TERM_CMD_SET);
    out (TERM_FLAG_CURSOR);
    return f;
}

void
con_set (char f)
{   
    out (TERM_CMD_CLR);
    out (255);
    out (TERM_CMD_SET);
    out (f);
}
