#include "libgfx.h"

void
error_out_of_heap_memory ()
{
    gfx_clear_screen (1);
    gfx_set_font (charset_4x8, 2);
    gfx_set_pencil_mode (1);
    gfx_draw_text (0, 84, "Error: Out of heap memory.");
    while (1);
}
