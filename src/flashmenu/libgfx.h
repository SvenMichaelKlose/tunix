#ifndef LIBGFX_H
#define LIBGFX_H

void gfx_init ();
short gfx_x () {}
void __fastcall__ gfx_clear_screen (char colour);
void gfx_reset_region ();
void __fastcall__ gfx_set_region (short x, short y, short w, short h);
void __fastcall__ gfx_draw_hline (short x, short y, short w, char colour);
void __fastcall__ gfx_draw_vline (short x, short y, short h, char colour);
void __fastcall__ gfx_draw_frame (short x, short y, short w, short h, char colour);
void __fastcall__ gfx_draw_box (short x, short y, short w, short h, char colour);
void __fastcall__ gfx_draw_text (short x, short y, char * txt, char colour);
void __fastcall__ gfx_set_pattern (void *);

#endif /* #ifndef LIBGFX_H */
