#ifndef LIBGFX_H
#define LIBGFX_H

extern void gfx_init ();
extern short gfx_x ();
extern void __fastcall__ gfx_clear_screen (char colour);
extern void gfx_reset_region ();
extern void __fastcall__ gfx_set_region (short x, short y, short w, short h);
extern void __fastcall__ gfx_draw_hline (short x, short y, short w);
extern void __fastcall__ gfx_draw_vline (short x, short y, short h);
extern void __fastcall__ gfx_draw_frame (short x, short y, short w, short h);
extern void __fastcall__ gfx_draw_box (short x, short y, short w, short h);
extern void __fastcall__ gfx_draw_text (short x, short y, char * txt);
extern void __fastcall__ gfx_set_pattern (void *);

extern char pattern_empty[8];
extern char pattern_solid[8];
extern char pattern_leaves[8];
extern char pattern_ovals[8];
extern char pattern_woven[8];
extern char pattern_pits[8];
extern char pattern_gray[8];
extern char pattern_smileys[8];

#endif /* #ifndef LIBGFX_H */
