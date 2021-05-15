.export _ultimem_send_command
.export _ultimem_poll

.proc _ultimem_send_command
    ldx #$aa
    stx $aaaa
    ldx #$55
    stx $a555
    sta $aaaa
    rts
.endproc

.proc _ultimem_poll
    lda $a000
    eor $a000
    and #$40
    bne _ultimem_poll
    rts
.endproc
