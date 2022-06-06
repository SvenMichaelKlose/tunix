.export main
.exportzp tmp3

.importzp s, d, c, p, tmp
.importzp name, namelen
.importzp xpos, ypos
.importzp font, font_bank
.importzp pencil_mode

.import ultifs_enter_root, ultifs_enter, ultifs_load
.import _ultimem_unhide
.import restore_state
.import init_alloc
.import init_copy_bank
.import init_ultifs_dev
.import gfx_init
.import clear_screen
.import putstring
.import moveram
.import _term_init, _term_puts, _term_put, _term_get

.include "../lib/term/libterm.inc"

.zeropage

tmp3:   .res 1

.code

membot  = $282      ; start page of BASIC RAM
memtop  = $284      ; end page of BASIC RAM
screen  = $288      ; start page of text matrix

.proc main
    sei
    lda #$7f
    sta $911d
    sta $911e
    cld
    ldx #$ff
    txs

    jsr _ultimem_unhide
    lda #%00000001  ; Ultimem LED on.
    sta $9ff0

    ; Restore saved state unless switch1 is being pressed.
    lda $9ff0
    and #%00000100  ; switch1
    beq no_restore
    ;jsr restore_state
no_restore:

;    jsr init_alloc      ; RAM bank allocator.
;    jsr init_proc       ; Process manager.
;    jsr init_copy_bank  ; Fast bank copy from $4000 to $2000.

    ; No expanded RAM.
    lda #%00000000
    sta $9ff1
    lda #%01000000
    sta $9ff2

    lda #$1e
    sta $288        ; Screen start
    lda #$10
    sta $282        ; BASIC start
    lda #$1e
    sta $284        ; BASIC end
    jsr $ff8a       ; KERNAL jump vectors.
    jsr $fdf9       ; VIAs.
    ;jsr $e518       ; VIC.
    ;jsr $e45b       ; BASIC jump vectors.
    ;jsr $e3a4       ; BASIC zero page.
    cli
    ;jmp $e378   ; BASIC cold start

    ; Move from ROM to RAM.
    lda #%01110000
    sta $9ff2
    lda #$a0
    sta s+1
    lda #$60
    sta d+1
    lda #$20
    sta c+1
    lda #0
    sta $9ffc
    sta s
    sta d
    sta c
    jsr moveram
    lda #%11111111
    sta $9ff2

    ; Activate all RAM below $8000.
    ldx #1
    stx $9ff4
    inx
    stx $9ff6
    inx
    stx $9ff8
    inx
    stx $9ffa
    inx
    stx $9ffc
    lda #%00111111
    sta $9ff1
    lda #%11111111
    sta $9ff2

    jsr _term_init
    lda #8
    sta $900f

w:  lda #<txt_welcome
    ldx #>txt_welcome
    jsr _term_puts

    jsr _term_get
    lda #TERM_CLEAR_SCREEN
    jsr _term_put
    jmp w
.endproc

.proc shift_font
    ldx #$01
    lda #$41
    sta c+1
    lda #0
    sta s
    lda font+1
    sta s+1
    ldy font
l:  lda (s),y
    asl
    asl
    asl
    asl
    sta (s),y
    iny
    bne n
    inc s+1
n:  dex
    bne l
    dec c+1
    bne l
    rts
.endproc

.rodata

txt_welcome:
    .byte TERM_ESCAPE, TERM_DISABLE_ATTR, TERM_ATTR_CURSOR
    .byte TERM_SET_CURSOR, 11, 1
    .byte TERM_ESCAPE, TERM_ENABLE_ATTR, TERM_ATTR_REVERSE
    .byte "Ultimate Ultiboot", 10, 13
    .byte TERM_ESCAPE, TERM_DISABLE_ATTR, TERM_ATTR_REVERSE
    .byte 10,13
    .byte "A:  Auto-detect", 10, 13
    .byte "0:  unexpanded", 10, 13
    .byte "1:  BLK1", 10, 13
    .byte "2:  BLK2", 10, 13
    .byte "3:  BLK3", 10, 13
    .byte "4:  IO2,3", 10, 13
    .byte "5:  BLK5", 10, 13
    .byte "6:  RAM1,2,3", 10, 13
    .byte "7:  +35K", 10, 13
    .byte "8:  +37K", 10, 13
    .byte 10,13
    .byte "B:  BASIC", 10, 13
    .byte "F2: VFORTH", 10, 13
    .byte "A:  Autostart from device.", 10, 13
    .byte "D:  device.", 10, 13
    .byte "M:  Memorize selection for boot.", 10, 13
    .byte 0

;    .include "../lib/term/charset-4x8.asm"
