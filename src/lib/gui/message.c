#include <stdio.h>

#include "libgfx.h"
#include "obj.h"
#include "message.h"

void __fastcall__
print_message (char * text)
{
    gfx_push_context ();
    gfx_reset_region ();
    gfx_set_font (charset_4x8, 2, FONT_BANK);
    gfx_set_pencil_mode (1);
    gfx_set_pattern (pattern_empty);
    gfx_draw_box (0, 23 * 8 - 1, 160, 9);
    gfx_set_pattern (pattern_solid);
    gfx_draw_hline (0, 23 * 8 - 2, 20 * 8);
    gfx_draw_text (0, 23 * 8, text);
    gfx_pop_context ();
}
