#ifndef LIBGFX_H
#define LIBGFX_H

#define PENCIL_MODE_OFF     0
#define PENCIL_MODE_OR      1

typedef short gpos;
typedef short gsize;
typedef unsigned char gcolor;

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
extern gpos gfx_x ();
extern void __fastcall__ gfx_clear_screen (gcolor);
extern void gfx_reset_region ();
extern void __fastcall__ gfx_set_region (gpos x, gpos y, gsize w, gsize h);
extern void __fastcall__ gfx_set_pencil_mode (char);
extern void __fastcall__ gfx_set_pattern (void *);
extern void __fastcall__ gfx_draw_hline (gpos x, gpos y, gsize w);
extern void __fastcall__ gfx_draw_vline (gpos x, gpos y, gsize h);
extern void __fastcall__ gfx_draw_frame (gpos x, gpos y, gsize w, gsize h);
extern void __fastcall__ gfx_draw_box (gpos x, gpos y, gsize w, gsize h);
extern void __fastcall__ gfx_set_font (void *, char space_size);
extern void __fastcall__ gfx_draw_text (gpos x, gpos y, char * txt);
extern gsize __fastcall__ gfx_get_text_width (char *);

#endif /* #ifndef LIBGFX_H */
