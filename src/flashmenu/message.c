#include <stdio.h>

#include "libgfx.h"
#include "obj.h"
#include "message.h"

gpos my = 0;
void
print_message (char * text)
{
    gfx_push_region ();
    gfx_reset_region ();
    gfx_set_font (charset_4x8, 2);
    gfx_set_pencil_mode (1);
    gfx_set_pattern (pattern_empty);
    gfx_draw_box (0, my, 160, 8);
    gfx_set_pattern (pattern_solid);
    gfx_draw_text (0, my, text);
    gfx_pop_region ();
    my += 8;
}

char msg[256];

void
print_obj (struct obj * o)
{
    sprintf (msg, "cd: %d %d %d %d - r: %d %d %d %d",
                  o->rect.x, o->rect.y, o->rect.w, o->rect.h,
                  gfx_rxl (), gfx_ryt (), gfx_rxr (), gfx_ryb ());
    print_message (msg);
}
