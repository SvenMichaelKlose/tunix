; UltiMem burn utility
; Written by Sven Michael Klose <pixel@hugbox,org>

__VIC20__ = 1
.include "cbm_kernal.inc"

.export _main, argument
.import _ultimem_erase_chip
.import ultimem_burn_byte
.import _ultimem_unhide
.import pushax
.importzp tmp, ptr, printptr, bnk


GETLIN = $c560


.code

.proc _main
    lda #<txt_welcome
    ldy #>txt_welcome
    jsr printstr

    jsr _ultimem_unhide
    cmp #$11
    beq has_ultimem

    lda #<txt_no_ultimem
    ldy #>txt_no_ultimem
    jmp printstr

has_ultimem:
    lda $9ff2
    and #%00111111
    ora #%01000000  ; ROM in BLK5.
    sta $9ff2

    jsr get_argument
    lda #','
    sta argument,y
    iny
    lda #'S'
    sta argument,y
    iny
    lda #','
    sta argument,y
    iny
    lda #'R'
    sta argument,y
    iny
    tya
    pha

    lda #<txt_erasing
    ldy #>txt_erasing
    jsr printstr
    jsr _ultimem_erase_chip

wait4chip:
    lda $a000
    cmp #$ff
    bne wait4chip

    lda #<txt_chip_erased
    ldy #>txt_chip_erased
    jsr printstr

    lda #<txt_burning
    ldy #>txt_burning
    jsr printstr

    lda #2
    ldx #8
    ldy #2
    jsr SETLFS
    pla
    ldx #<argument
    ldy #>argument
    jsr SETNAM
    jsr OPEN
    ldx #2
    jsr CHKIN

    lda #0
    sta bnk
    sta bnk+1

l2: lda #$00
    sta ptr
    lda #$a0
    sta ptr+1
    lda bnk
    sta $9ffe
    lda bnk+1
    sta $9fff

l:  jsr BASIN
    sta tmp

;    ldy #0
;    lda (ptr),y
;    cmp tmp
;    beq dont_burn

    lda ptr
    ldx ptr+1
    ldy tmp
    jsr ultimem_burn_byte

;    ldy #0
;    lda (ptr),y
;    cmp tmp
;    bne err_not_burned

dont_burn:
    jsr READST
    cmp #$40
    beq done
    cmp #0
    bne error

    inc ptr
    bne l

    lda ptr+1
    and #%00011111
    bne no_dot
    lda #$2e
    jsr BSOUT
no_dot:

    inc ptr+1
    lda ptr+1
    cmp #$c0
    bne l

    inc bnk
    bne l2
    inc bnk+1
    jmp l2

done:
    lda #<txt_done
    ldy #>txt_done
    jsr printstr

exit:
    jsr CLRCH
    lda #2
    jmp CLOSE

error:
    lda #<txt_error
    ldy #>txt_error
    jsr printstr
    jmp exit

err_not_burned:
    lda #<txt_error_not_burned
    ldy #>txt_error_not_burned
    jsr printstr
    jmp exit
.endproc

.proc printstr
    sta printptr
    sty printptr+1

l:  ldy #0
    lda (printptr),y
    beq done
    jsr BSOUT
    inc printptr
    bne l
    inc printptr+1
    bne l

done:
    rts
.endproc

.proc get_argument
    lda #<argument
    sta ptr
    lda #>argument
    sta ptr+1

    ; Check on REM argument after RUN.
    lda $0200
    cmp #$8a
    bne get_user_input
    lda $0201
    cmp #$3a
    bne get_user_input
    lda $0202
    cmp #$8f
    bne get_user_input

    ; Skip optional spaces after REM.
    ldy #3
l:  lda $200,y
    iny
    cmp #$20
    beq l
    dey

    ; Copy argument.
copy:
    lda $200,y
    tax
    tya
    pha
    ldy #0
    txa
    sta (ptr),y
    inc ptr
    bne n2
    inc ptr+1
n2: pla
    tay
    iny
    cpx #0
    bne copy

    ldy #0
l2: lda argument,y
    beq got_len 
    iny
    jmp l2

got_len:
    rts

get_user_input:
    lda #<txt_enter_filename
    ldy #>txt_enter_filename
    jsr printstr
    lda #0
    sta $200
    jsr GETLIN
    ldy #0
    jmp copy
.endproc


    .data

txt_welcome:
    .byte $93
    .byte "ULTIMEM ROM BURNER", 13
    .byte "BY PIXEL@HUGBOX.ORG", 13
    .byte 13
    .byte "FOR MORE INFO PLEASE", 13
    .byte "VISIT VIC DENIAL AND", 13
    .byte "RETRO INNOVATIONS.", 13
    .byte 0

txt_no_ultimem:
    .byte "NO ULTIMEM FOUND", 13
    .byte "EXITING.", 13
    .byte 0

txt_enter_filename:
    .byte 13, 13
    .byte "NO FILENAME PASSED IN", 13
    .byte "REM AFTER RUN, LIKE", 13
    .byte 13
    .byte " RUN:REM EXAMPLE.IMG",13
    .byte 13
    .byte "PLEASE ENTER THE DUMP", 13
    .byte "FILE NAME NOW - THIS", 13
    .byte "WILL DESTROY ALL DATA", 13
    .byte "ON THE FLASH ROM RIGHT" ;, 13
    .byte "AWAY!: ", 13
    .byte 0

txt_erasing:
    .byte "ERASING ROM...", 0

txt_chip_erased:
    .byte "OK.", 13,0

txt_burning:
    .byte "BURNING...", 13, 0

txt_done:
    .byte "DONE.", 13,0

txt_error:
    .byte "FILE ERROR.", 13,0

txt_error_not_burned:
    .byte "ERROR: BYTE HAS NOT BEEN WRITTEN.", 13,0

    .bss

argument:   .res 64
