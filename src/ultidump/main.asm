__VIC20__ = 1
.include "cbm_kernal.inc"

.export _main
.import _ultimem_erase_chip
.import ultimem_burn_byte
.import _ultimem_unhide


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
    lda #0
    sta $9ffe
    sta $9fff

    lda #<txt_dumping
    ldy #>txt_dumping
    jsr printstr

    lda #2
    ldx #8
    ldy #2
    jsr SETLFS
    lda #fn_data_end-fn_data
    ldx #<fn_data
    ldy #>fn_data
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

    .data

txt_welcome:
    .byte $93, "ULTIDUMP", 13, 0

txt_no_ultimem:
    .byte "NO ULTIMEM FOUND", 13
    .byte "EXITING.", 13,0

txt_dumping:
    .byte "DUMPING TO 'DUMP.IMG':", 13, 0

txt_dot:
    .byte ".", 0

txt_done:
    .byte "DONE.", 13,0

txt_error:
    .byte "FILE ERROR.", 13,0

fn_data:
    .byte "DUMP.IMG,S,W"
fn_data_end:
