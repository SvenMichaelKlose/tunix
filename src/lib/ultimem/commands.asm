.export _ultimem_send_command
.export _ultimem_burn_byte
.export _ultimem_erase_chip
.export _ultimem_erase_block

.importzp s, d
.import popax

.proc _ultimem_send_command
    ldx #$aa
    stx $aaaa
    ldx #$55
    stx $a555
    sta $aaaa
    rts
.endproc

.proc _ultimem_burn_byte
    sta d
    jsr popax
    sta s
    stx s+1
    lda #$a0
    jsr _ultimem_send_command
    lda d
    ldx #0
    sta (s,x)
    jmp poll
.endproc

.proc _ultimem_erase_chip
    lda #$80
    jsr _ultimem_send_command
    lda #$10
    jsr _ultimem_send_command
    jmp poll
.endproc

.proc poll
l:  lda $a000
    eor $a000
    and #$80
    bne l
    rts
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
    jmp poll
.endproc
