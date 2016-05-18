.export _gfx_init = gfx_init
.export _gfx_x
.export _gfx_rxl
.export _gfx_ryt
.export _gfx_rxr
.export _gfx_ryb
.export _gfx_clear_screen = clear_screen
.export _gfx_reset_region = reset_region
.export _gfx_set_region
.export _gfx_set_pencil_mode
.export _gfx_set_pattern
.export _gfx_set_screen_base
.export _gfx_set_position
.export _gfx_draw_hline
.export _gfx_draw_vline
.export _gfx_draw_frame
.export _gfx_draw_box
.export _gfx_set_font
.export _gfx_draw_text
.export _gfx_putchar_fixed = putchar_fixed
.export _gfx_get_text_width
.export _gfx_push_context = push_context
.export _gfx_pop_context = pop_context

.importzp xpos, ypos, width, height, color, rxr, rxl, ryt, ryb, p, s
.importzp pattern, font, pencil_mode, font_space_size
.importzp scrbase
.import popax
.import gfx_init, clear_screen, reset_region, hline, vline, frame, box, putstring, get_text_width
.import putchar_fixed
.import push_context, pop_context

.code

; gpos gfx_x ();
.proc _gfx_x
    lda xpos
    ldx #0
    rts
.endproc

; gpos gfx_rxl ();
.proc _gfx_rxl
    lda rxl
    ldx #0
    rts
.endproc

; gpos gfx_ryt ();
.proc _gfx_ryt
    lda ryt
    ldx #0
    rts
.endproc

; gpos gfx_rxr ();
.proc _gfx_rxr
    lda rxr
    ldx #0
    rts
.endproc

; gpos gfx_ryb ();
.proc _gfx_ryb
    lda ryb
    ldx #0
    rts
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

; void __fastcall__ gfx_set_screen_base (unsigned short);
.proc _gfx_set_screen_base
    sta scrbase
    stx scrbase+1
	rts
.endproc

; void __fastcall__ gfx_set_position (gpos x, gpos y);
.proc _gfx_set_position
	sta ypos
    jsr popax
	sta xpos
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
    sta p
    stx p+1
    jsr get_text_width
    ldx #0
	rts
.endproc
