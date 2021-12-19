__VIC20__ = 1
.include "cbm_kernal.inc"

.export _main
.import _ultimem_erase_chip
.import ultimem_burn_byte
.import _ultimem_unhide
.import pushax
.importzp tmp, ptr, cnt

.code

.proc _main
    lda #<txt_welcome
    ldy #>txt_welcome
    jsr $cb1e

    jsr _ultimem_unhide
    cmp #$11
    beq has_ultimem

    lda #<txt_no_ultimem
    ldy #>txt_no_ultimem
    jmp $cb1e

has_ultimem:
    lda $9ff2
    and #%00111111
    ora #%01000000  ; ROM in BLK5.
    sta $9ff2
    lda #0
    sta $9ffe
    sta $9fff
    sta cnt
    sta cnt+1

    lda #<txt_dumping
    ldy #>txt_dumping
    jsr $cb1e

    lda #2
    ldx #8
    ldy #2
    jsr SETLFS
    lda #fn_data_end-fn_data
    ldx #<fn_data
    ldy #>fn_data
    jsr SETNAM
    jsr OPEN
    ldx #2
    jsr CHKOUT

    lda #0
    sta $9ffe
    sta $9fff

l2: lda #$00
    sta ptr
    lda #$a0
    sta ptr+1

l:  ldy #0
    lda (ptr),y
    jsr BSOUT

    lda $900f
    clc
    adc #1
    and #7
    ora #$18
    sta $900f

    inc ptr
    bne l

    inc cnt
    bne l3
    inc cnt+1
l3: lda cnt
    cmp #<1489
    bne no_dot
    lda cnt+1
    cmp #>1489
    bne no_dot
    ldx #0
    stx cnt
    stx cnt+1
    jsr CHKOUT
    lda #<txt_dot
    ldy #>txt_dot
    jsr $cb1e
    ldx #2
    jsr CHKOUT
no_dot:

    inc ptr+1
    lda ptr+1
    cmp #$c0
    bne l

    inc $9ffe
    bne l2
    inc $9fff
    lda $9fff
    cmp #4
    bne l2

done:
    ldx #0
    jsr CHKOUT
    lda #<txt_done
    ldy #>txt_done
    jsr $cb1e

exit:
    jsr CLRCH
    lda #2
    jsr CLOSE

    rts

error:
    lda #<txt_error
    ldy #>txt_error
    jsr $cb1e
    jmp exit
.endproc

txt_welcome:
    .byte "ULTIMEM ROM DUMP.", 13, 0

txt_no_ultimem:
    .byte "NO ULTIMEM AROUND - EXITING.", 13,0

txt_dumping:
    .byte $93, "DUMPING TO 'DUMP.IMG':", 13, 0

txt_dot:
    .byte ".", 0

txt_done:
    .byte "DONE.", 13,0

txt_error:
    .byte "FILE ERROR.", 13,0

fn_data:
    .byte "DUMP.IMG,S,W"
fn_data_end:
