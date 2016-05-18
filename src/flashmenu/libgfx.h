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

extern void gfx_init (void);
extern gpos gfx_x (void);
extern gpos gfx_rxl (void);
extern gpos gfx_ryt (void);
extern gpos gfx_rxr (void);
extern gpos gfx_ryb (void);
extern void __fastcall__ gfx_clear_screen (gcolor);
extern void gfx_reset_region (void);
extern void __fastcall__ gfx_set_region (gpos x, gpos y, gsize w, gsize h);
extern void gfx_push_context (void);
extern void gfx_pop_context (void);
extern void __fastcall__ gfx_set_pencil_mode (char);
extern void __fastcall__ gfx_set_pattern (void *);
extern void __fastcall__ gfx_set_screen_base (unsigned short);
extern void __fastcall__ gfx_set_position (gpos x, gpos y);
extern void __fastcall__ gfx_draw_hline (gpos x, gpos y, gsize w);
extern void __fastcall__ gfx_draw_vline (gpos x, gpos y, gsize h);
extern void __fastcall__ gfx_draw_frame (gpos x, gpos y, gsize w, gsize h);
extern void __fastcall__ gfx_draw_box (gpos x, gpos y, gsize w, gsize h);
extern void __fastcall__ gfx_set_font (void *, char space_size);
extern void __fastcall__ gfx_draw_text (gpos x, gpos y, char * txt);
extern void __fastcall__ gfx_putchar_fixed (char);
extern gsize __fastcall__ gfx_get_text_width (char *);
extern void __fastcall__ gfx_copy_area (unsigned short sbase, unsigned short dbase, gpos sx, gpos sy, gpos dx, gpos dy, gsize width, gsize height);

#endif /* #ifndef LIBGFX_H */
