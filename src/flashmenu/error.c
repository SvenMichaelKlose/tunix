#include "libgfx.h"

void
print_error (char * text)
{
    gfx_clear_screen (1);
    gfx_set_font (charset_4x8, 2);
    gfx_set_pencil_mode (1);
    gfx_draw_text (0, 84, text);
    while (1);
}

void
error_out_of_heap_memory ()
{
    print_error ("Error: Out of heap memory.");
}
