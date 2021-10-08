.export _ultimem_erase_chip
.export _ultimem_erase_block

.importzp s
.import _ultimem_send_command
.import _ultimem_poll

.proc _ultimem_erase_chip
    lda #$80
    jsr _ultimem_send_command
    lda #$10
    jmp _ultimem_send_command
.endproc

.proc _ultimem_erase_block
    ldx #0
    stx s
    asl
    rol s
    asl
    rol s
    asl
    rol s
    sta $9ffe
    lda s
    sta $9fff

    lda #$80
    jsr _ultimem_send_command
    lda #$30
    jsr _ultimem_send_command
    jmp _ultimem_poll
.endproc
