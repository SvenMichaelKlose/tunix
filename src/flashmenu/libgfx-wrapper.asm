.export _gfx_draw_hline

.importzp xpos, ypos, width, color
.import popax
.import gfx_init, hline

.code

.proc _gfx_init
	jmp gfx_init
.endproc

.proc _gfx_draw_hline
    ;sta color
    jsr popax
    sta width
    jsr popax
    sta ypos
    jsr popax
    sta xpos
    jmp hline
.endproc
