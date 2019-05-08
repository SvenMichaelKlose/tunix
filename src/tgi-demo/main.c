#include "g.h"

#include <conio.h>
#include "tgi.h"

#include "main.h"

const unsigned char bw_palette[2] = { TGI_COLOR_BLACK, TGI_COLOR_WHITE };

int
main (int argc, char ** argv)
{
    int a;
    int max_x;
    int max_y;

    tgi_install (tgi_static_stddrv);

    tgi_init ();
    tgi_clear ();
    tgi_setpalette (bw_palette);
    bordercolor (COLOR_GREEN);
    max_x = tgi_getmaxx () + 1;
    max_y = tgi_getmaxy () + 1;
    tgi_setcolor (COLOR_WHITE);
    for (a = 0; a < max_x; a++)
        tgi_line (0, 0, a, max_y);
    for (a = 0; a < max_y; a++)
        tgi_line (0, 0, max_x, a);
    tgi_setcolor (COLOR_BLACK);
    for (a = 0; a < max_x / 2; a++)
        tgi_circle (max_x / 2, max_y / 2, a);
    tgi_setcolor (COLOR_WHITE);
    tgi_bar (0, 0, max_x, max_y);

    tgi_done ();

    return 0;
}
