.export _gfx_init
.export _gfx_x
.export _gfx_clear_screen
.export _gfx_reset_region
.export _gfx_set_region
.export _gfx_set_pencil_mode
.export _gfx_set_pattern
.export _gfx_draw_hline
.export _gfx_draw_vline
.export _gfx_draw_frame
.export _gfx_draw_box
.export _gfx_set_font
.export _gfx_draw_text
.export _gfx_get_text_width

.importzp xpos, ypos, width, height, color, rxr, rxl, ryt, ryb, p, s
.importzp pattern, font, pencil_mode, font_space_size
.import popax
.import gfx_init, clear_screen, reset_region, hline, vline, frame, box, putstring, get_text_width

.code

; void gfx_init ()
.proc _gfx_init
	jmp gfx_init
.endproc

; gpos gfx_x ();
.proc _gfx_x
    lda xpos
    ldx #0
    rts
.endproc

; void __fastcall__ gfx_clear_screen (gcolor);
.proc _gfx_clear_screen
	jmp clear_screen
.endproc

; void gfx_reset_region ()
.proc _gfx_reset_region
	jmp reset_region
.endproc

; void __fastcall__ gfx_set_region (gpos x, gpos y, gsize w, gsize h) {}
.proc _gfx_set_region
	sta ryb
    jsr popax
	sta rxr
    jsr popax
	sta ryt
	clc
	adc ryb
	sta ryb
    jsr popax
	sta rxl
	clc
	adc rxr
	sta rxr
	rts
.endproc

; void __fastcall__ gfx_set_pencil_mode (char);
.proc _gfx_set_pencil_mode
    sta pencil_mode
	rts
.endproc

; void __fastcall__ gfx_set_pattern (void *);
.proc _gfx_set_pattern
    sta pattern
    stx pattern+1
	rts
.endproc

; void __fastcall__ gfx_draw_hline (gpos x, gpos y, gsize w);
.proc _gfx_draw_hline
    sta width
    jsr popax
    sta ypos
    jsr popax
    sta xpos
    jmp hline
.endproc

; void __fastcall__ gfx_draw_vline (gpos x, gpos y, gsize h);
.proc _gfx_draw_vline
    sta height
    jsr popax
    sta ypos
    jsr popax
    sta xpos
    jmp vline
.endproc

; void __fastcall__ gfx_draw_frame (gpos x, gpos y, gsize w, gsize h);
.proc _gfx_draw_frame
    sta height
    jsr popax
    sta width
    jsr popax
    sta ypos
    jsr popax
    sta xpos
    jmp frame
.endproc

; void __fastcall__ gfx_draw_box (gpos x, gpos y, gsize w, gsize h);
.proc _gfx_draw_box
    sta height
    jsr popax
    sta width
    jsr popax
    sta ypos
    jsr popax
    sta xpos
    jmp box
.endproc

; void __fastcall__ gfx_set_font (void *, char space_size);
.proc _gfx_set_font
    sta font_space_size
    jsr popax
    sta font
    stx font+1
	rts
.endproc

; void __fastcall__ gfx_draw_text (gpos x, gpos y, char * txt);
.proc _gfx_draw_text
    sta p
	stx p+1
    jsr popax
    sta ypos
    jsr popax
    sta xpos
    jmp putstring
.endproc

; gsize __fastcall__ gfx_get_text_size (char *);
.proc _gfx_get_text_width
    sta s
    stx s+1
    jsr get_text_width
    ldx #0
	rts
.endproc
