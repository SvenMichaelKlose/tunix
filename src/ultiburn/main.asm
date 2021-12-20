; UltiMem burn utility
; Written by Sven Michael Klose <pixel@hugbox,org>

__VIC20__ = 1
.include "cbm_kernal.inc"

.export _main
.import _ultimem_erase_chip
.import ultimem_burn_byte
.import _ultimem_unhide
.import pushax
.importzp tmp, ptr, printptr, bnk

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

    lda #<txt_installing
    ldy #>txt_installing
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

txt_welcome:
    .byte "ULTIBURN", 13, 0

txt_no_ultimem:
    .byte "NO ULTIMEM AROUND - EXITING.", 13,0

txt_erasing:
    .byte "ERASING ROM...", 0

txt_chip_erased:
    .byte "OK.", 13,0

txt_installing:
    .byte "BURNING", 0

txt_done:
    .byte "DONE.", 13,0

txt_error:
    .byte "FILE ERROR.", 13,0

txt_error_not_burned:
    .byte "ERROR: BYTE HAS NOT BEEN WRITTEN.", 13,0

fn_data:
    .byte "IMAGE,S,R"
fn_data_end:
