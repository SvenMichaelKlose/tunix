.export main, _reset
.exportzp tmp3
.export popax

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
.import term_init, _term_puts, _term_put, _term_get

.include "../lib/term/libterm.inc"

.zeropage

tmp3:   .res 1

.code

membot  = $282      ; start page of BASIC RAM
memtop  = $284      ; end page of BASIC RAM
screen  = $288      ; start page of text matrix

.proc popax
.endproc

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

    ; Move from ROM to RAM.
;    lda #%01110000
;    sta $9ff2
;    lda #$a0
;    sta s+1
;    lda #$60
;    sta d+1
;    lda #$20
;    sta c+1
;    lda #0
;    sta $9ffc
;    sta s
;    sta d
;    sta c
;    jsr moveram
;    lda #%11111111
;    sta $9ff2

    jmp _reset

    lda #1
    ldx #0
    ldy #2
    jsr term_init

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
    .byte "ULTIMATE ULTIBOOT", 10, 13
    .byte TERM_ESCAPE, TERM_DISABLE_ATTR, TERM_ATTR_REVERSE
    .byte 10,13
    .byte "M: Memorize  B: BASIC  R: ROM  D:Device", 10, 13
    .byte 10,13
    .byte "A: Auto-detect  0: Unexpanded  1: BLK1", 10, 13
    .byte "2: BLK2  3: BLK3  4: IO2,3  5: BLK5", 10, 13
    .byte "6: RAM1,2,3  7: +35K  8: +37K", 10, 13
    .byte 10,13
    .byte 0

txt_roms:       .byte "ROMs", 0
txt_devs:       .byte "Devices", 0

;    .include "../lib/term/charset-4x8.asm"

.proc _reset
    ; Configure RAM blocks.
    lda #%00111111
    sta $9ff1
    ldx #2
    lda #0
    stx $9ff4
    sta $9ff5
    inx
    stx $9ff6
    sta $9ff7
    inx
    stx $9ff8
    sta $9ff9
    inx
    stx $9ffa
    sta $9ffb
    inx
    stx $9ffc
    sta $9ffd
    inx

    ; Copy rest of init code to IO2,3.
    ldy #end - start
l:  lda start,y
    sta $9d00,y
    dey
    bpl l
    jmp $9d00

start:
    lda #0
    stx $9ffe
    sta $9fff
    lda #%11111111
    sta $9ff2

READY     := $C474
PRNTCRLF  := $CAD7
PTRSTR    := $CB1E
INITBA    := $E3A4
INITVCTRS := $E45B
FREMSG    := $E404
INITSK    := $E518
INITMEM   := $FD8D
FRESTOR   := $FD52
INITVIA   := $FDF9

    cld
    jsr INITMEM     ; Init memory
    jsr FRESTOR     ; I/O vectors
    jsr INITVIA     ; VIAs
    jsr INITSK      ; VIC & clear screen
    jsr INITVCTRS   ; BASIC vectors
    jsr INITBA      ; BASIC zero page
    jsr FREMSG      ; Print welcome message
    jmp READY       ;jmp $e381       ; BASIC cold start
end:
.endproc
