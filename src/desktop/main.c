#include <lib/ingle/cc65-charmap.h>

#include <cbm.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <conio.h>

#include <lib/ingle/ingle.h>
#include <lib/ultimem/ultimem.h>
#include <lib/gfx/libgfx.h>
#include <lib/gui/obj.h>
#include <lib/gui/error.h>
#include <lib/gui/message.h>
#include <lib/gui/obj.h>
#include <lib/gui/box.h>
#include <lib/gui/window.h>

#include "desktop.h"

void
init_memory ()
{
    * (char *) 0x9ff2 = 0xff;
    *ULTIMEM_BLK5 = *ULTIMEM_BLK3 + 1;
    _heapadd ((void *) 0xa000, 0x2000); /* BANK5 */
    _heapadd ((void *) 0x400, 0xc00);   /* +3K */
}

void
shift_charset ()
{
    short old_bank = *ULTIMEM_BLK5;
    int i;
    char * charset = (char *) 0xa000;

    *ULTIMEM_BLK5 = FONT_BANK;
    for (i = 0; i < 2048; i++)
        charset[i] <<= 4;
    *ULTIMEM_BLK5 = old_bank;
}

void
init_console ()
{
    shift_charset ();
    gfx_clear_screen (0);
    gfx_init ();
    gfx_set_font (charset_4x8, 2, FONT_BANK);
}

int
main (int argc, char ** argv)
{
    (void) argc;
    (void) argv;

    init_memory ();
    init_console ();
    start_desktop ();

    return 0;
}
