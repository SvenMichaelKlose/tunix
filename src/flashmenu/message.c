#include "libgfx.h"
#include "message.h"

void
print_message (char * text)
{
    gfx_push_region ();
    gfx_reset_region ();
    gfx_set_font (charset_4x8, 2);
    gfx_set_pencil_mode (1);
    gfx_set_pattern (pattern_empty);
    gfx_draw_box (0, 23 * 8, 160, 8);
    gfx_set_pattern (pattern_solid);
    gfx_draw_text (0, 23 * 8, text);
    gfx_pop_region ();
}
