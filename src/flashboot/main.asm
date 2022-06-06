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
.import _term_init, _term_puts

.include "../lib/term/libterm.inc"

.zeropage

tmp3:   .res 1

.code

membot  = $282      ; start page of BASIC RAM
memtop  = $284      ; end page of BASIC RAM
screen  = $288      ; start page of text matrix

.proc main
    ; Don't get interrupted.
    sei
    lda #$7f
    sta $911d
    sta $911e

    cld
    ldx #$ff
    txs

    lda #$22    ; Red screen.
    sta $900f
    lda #$00    ; Blank screen.
    sta $9002

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


;lda #%00111111
;sta $9ff1
;lda #%01111111
;sta $9ff2
    jsr $fd8d   ; Init memory.
    jsr $fd52   ; Init KERNAL.
    jsr $fdf9   ; Init VIAs.
;    jsr $e518   ; Init VIC.

;jmp $e378   ; BASIC cold start

    ; Activate all RAM below $8000.
    lda #%00111111
    sta $9ff1
    lda #%01111111
    sta $9ff2
    lda #0
    tax
    stx $9ff4
    sta $9ff5
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

    ; Move from ROM to RAM.
    lda #$a0
    sta s+1
    lda #$60
    sta d+1
    lda #$20
    sta c+1
    lda #0
    sta $9ffc
    sta $9ffd
    sta s
    sta d
    sta c
    jsr moveram
    lda #%11111111
    sta $9ff2

    ; Init screen.
;    jsr clear_screen
;    lda #0
;    ldx #1
;    ldy #2
;    jsr gfx_init
;    lda #0
;    sta xpos
;    sta ypos
;    sta font_bank
    lda #1
    sta pencil_mode

    ; Prepare font.
;    lda #<charset_4x8
;    sta font
;    lda #>charset_4x8
;    sta font+1
;    jsr shift_font

;    lda #<txt_welcome
;    sta p
;    lda #>txt_welcome
;    sta p+1
;    jsr putstring

    jsr _term_init
    lda #<txt_welcome
    ldx #>txt_welcome
    jsr _term_puts

w:  jmp w
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
    .byte "Ultimate Ultiboot", 0

;    .include "../lib/term/charset-4x8.asm"
