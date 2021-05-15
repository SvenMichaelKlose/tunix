.export _ultimem_is_installed
.export _ultimem_get_size

.import _ultimem_send_command
.importzp tmp

; Does not seem to work.
.proc _ultimem_is_installed
    lda #$90
    jsr _ultimem_send_command
    lda $a000
    sta tmp
    lda $a002
    pha
    lda #$f0
    jsr _ultimem_send_command
    pla
    cmp #$7e
    bne e
    lda tmp
    cmp #1
e:  rts
.endproc

; Not tested.
.proc _ultimem_get_size
    lda #$90
    jsr _ultimem_send_command
    ldx $a01c
    lda $a01e
    pha
    lda #$f0
    jsr _ultimem_send_command
    pla
    rts
.endproc
