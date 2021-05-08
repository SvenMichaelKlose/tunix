__VIC20__ = 1
.include "cbm_kernal.inc"

.export _main
.import _ultimem_erase_chip
.import _ultimem_burn_byte
.import _ultimem_is_installed
.import ultimem_unhide_regs
.import pushax
.importzp tmp

.data

ptr:    .res 2

.code

.proc _main
    lda #<txt_welcome
    ldy #>txt_welcome
    jsr $cb1e

    jsr _ultimem_is_installed
    beq has_ultimem

    lda #<txt_no_ultimem
    ldy #>txt_no_ultimem
    jmp $cb1e

has_ultimem:
    jsr ultimem_unhide_regs

    lda #%01000000          ; ROM in BLK5.
    sta $9ff1

    jsr _ultimem_erase_chip
    lda #<txt_chip_erased
    ldy #>txt_chip_erased
    jsr $cb1e

    lda #<txt_installing
    ldy #>txt_installing
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
    jsr CHKIN

    lda #0
    sta $9ffe
    sta $9fff

l2: lda #$00
    sta ptr
    lda #$a0
    sta ptr+1

l:  jsr BASIN
    sta tmp

    jsr READST
    cmp #$40
    beq done
    cmp #0
    bne error

    inc $900f
    lda ptr
    ldx ptr+1
    jsr pushax
    lda tmp
    jsr _ultimem_burn_byte

    inc ptr
    bne l

    lda #<txt_dot
    ldy #>txt_dot
    jsr $cb1e

    inc ptr+1
    lda ptr+1
    cmp #$c0
    bne l

    inc $9ffe
    bne l2
    inc $9fff
    jmp l2

done:
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
    .byte $93, "INGLE INSTALLER.", 13, 0

txt_no_ultimem:
    .byte "NO ULTIMEM AROUND - EXITING.", 13,0

txt_chip_erased:
    .byte "FLASH ROM ERASED.", 13,0

txt_installing:
    .byte "INSTALLING INGLE...", 13, 0

txt_dot:
    .byte ".", 0

txt_done:
    .byte "DONE.", 13,0

txt_error:
    .byte "ERROR.", 13,0

fn_data:
    .byte "0:INGLE.IMG,S,R"
fn_data_end:
