#include "cc65-charmap.h"

#include <cbm.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <conio.h>

#include <ultimem.h>
#include <ingle.h>
#include <libgfx.h>

#include "obj.h"
#include "event.h"
#include "box.h"
#include "button.h"
#include "error.h"
#include "frame.h"
#include "inputline.h"
#include "layout-ops.h"
#include "list.h"
#include "message.h"
#include "table.h"
#include "window.h"

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
