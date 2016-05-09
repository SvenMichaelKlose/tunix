#ifndef LIBGFX_H
#define LIBGFX_H

#define PENCIL_MODE_OFF     0
#define PENCIL_MODE_OR      1

extern char charset_4x8[2048];

extern char pattern_empty[8];
extern char pattern_solid[8];
extern char pattern_leaves[8];
extern char pattern_ovals[8];
extern char pattern_woven[8];
extern char pattern_pits[8];
extern char pattern_gray[8];
extern char pattern_smileys[8];

extern void gfx_init ();
extern short gfx_x ();
extern void __fastcall__ gfx_clear_screen (char colour);
extern void gfx_reset_region ();
extern void __fastcall__ gfx_set_region (short x, short y, short w, short h);
extern void __fastcall__ gfx_set_pencil_mode (char);
extern void __fastcall__ gfx_set_pattern (void *);
extern void __fastcall__ gfx_draw_hline (short x, short y, short w);
extern void __fastcall__ gfx_draw_vline (short x, short y, short h);
extern void __fastcall__ gfx_draw_frame (short x, short y, short w, short h);
extern void __fastcall__ gfx_draw_box (short x, short y, short w, short h);
extern void __fastcall__ gfx_set_font (void *, char space_size);
extern void __fastcall__ gfx_draw_text (short x, short y, char * txt);
extern short __fastcall__ gfx_get_text_width (char *);

#endif /* #ifndef LIBGFX_H */
