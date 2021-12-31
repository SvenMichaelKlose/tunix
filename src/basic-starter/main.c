#include <lib/ingle/cc65-charmap.h>

#include <cbm.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <conio.h>

#include <lib/ultimem/ultimem.h>
#include <lib/ingle/ingle.h>
#include <lib/gfx/libgfx.h>

#include <lib/gui/obj.h>
#include <lib/gui/event.h>
#include <lib/gui/box.h>
#include <lib/gui/button.h>
#include <lib/gui/error.h>
#include <lib/gui/frame.h>
#include <lib/gui/inputline.h>
#include <lib/gui/layout-ops.h>
#include <lib/gui/list.h>
#include <lib/gui/message.h>
#include <lib/gui/table.h>
#include <lib/gui/window.h>

//#include "basic-starter.h"
//#include "desktop.h"

int
main (void)
{
 //   struct obj * win = make_basic_starter ();
    print_message ("BASIC starter");

    //set_obj_position_and_size (win, 0, 0, DESKTOP_WIDTH, DESKTOP_HEIGHT);
    //focussed_window = append_obj (desktop, win);

    //print_message ("Basic starter Press 'H' for help.");
    //draw_obj (desktop);

    while (1); //desktop_loop ();

    return 0;
}
