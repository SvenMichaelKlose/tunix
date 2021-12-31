#include <stdio.h>

#include <lib/gfx/libgfx.h>
#include <lib/gui/obj.h>
#include <lib/gui/message.h>

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
    gfx_draw_text (1, 23 * 8, text);
    gfx_pop_context ();
}

char message_buffer[64];

void __fastcall__
print_obj (struct obj * o)
{
    sprintf (message_buffer, "cd: %d %d %d %d - r: %d %d %d %d",
                             o->rect.x, o->rect.y, o->rect.w, o->rect.h,
                             gfx_rxl (), gfx_ryt (), gfx_rxr (), gfx_ryb ());
    print_message (message_buffer);
}
