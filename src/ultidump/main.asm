; UltiMem ROM dump utility
; Written by Sven Michael Klose <pixel@hugbox.org>

__VIC20__ = 1
.include "cbm_kernal.inc"

.export _main

.import _ultimem_erase_chip
.import ultimem_burn_byte
.import _ultimem_unhide

GETLIN = $c560

.zeropage

ptr:        .res 2
cnt:        .res 2
bnk:        .res 2
printptr:   .res 2


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

    pha
    lda #<txt_dumping1
    ldy #>txt_dumping1
    jsr printstr
    lda #<argument
    ldy #>argument
    jsr printstr
    lda #<txt_dumping2
    ldy #>txt_dumping2
    jsr printstr
    pla
    tay

    lda #','
    sta argument,y
    iny
    lda #'S'
    sta argument,y
    iny
    lda #','
    sta argument,y
    iny
    lda #'W'
    sta argument,y
    iny
    tya
    pha
    lda #2
    ldx #8
    ldy #2
    jsr SETLFS
    pla
    ldx #<argument
    ldy #>argument
    jsr SETNAM
    jsr OPEN

    lda #0
    sta bnk
    sta bnk+1
    sta cnt
    sta cnt+1

    ldx #2
    jsr CHKOUT

l2: lda #$00
    sta ptr
    lda #$a0
    sta ptr+1

    lda bnk
    sta $9ffe
    lda bnk+1
    sta $9fff

l:  ldy #0
    lda (ptr),y
    jsr BSOUT

    inc ptr
    bne l

    inc cnt
    bne l3
    inc cnt+1
l3:

    lda cnt
    cmp #<1489
    bne no_dot
    lda cnt+1
    cmp #>1489
    bne no_dot

    ldx #0
    stx cnt
    stx cnt+1
    jsr CHKOUT
    lda #$2e
    jsr BSOUT
    ldx #2
    jsr CHKOUT
no_dot:

    inc ptr+1
    lda ptr+1
    cmp #$c0
    bne l

    inc bnk
    bne l2
    inc bnk+1
    lda bnk+1
    cmp #4
    bne l2

done:
    ldx #0
    jsr CHKOUT
    lda #<txt_done
    ldy #>txt_done
    jsr printstr

exit:
    ldx #2
    jsr CHKOUT
    jsr CLRCH
    lda #2
    jmp CLOSE

error:
    lda #<txt_error
    ldy #>txt_error
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
    tya
    rts

get_user_input:
    lda #<txt_enter_filename
    ldy #>txt_enter_filename
    jsr printstr
    jsr GETLIN
    ldy #0
    jmp copy
.endproc


    .data

txt_welcome:
    .byte $93, "ULTIDUMP", 13, 0

txt_no_ultimem:
    .byte "NO ULTIMEM FOUND", 13
    .byte "EXITING.", 13,0

txt_enter_filename:
    .byte "PLEASE ENTER DUMP FILENAME: ", 13, 0

txt_dumping1:
    .byte "DUMPING TO '", 0

txt_dumping2:
    .byte "':", 13, 0

txt_done:
    .byte "DONE.", 13,0

txt_error:
    .byte "FILE ERROR.", 13,0


    .bss

argument:   .res 64
