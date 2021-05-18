.export _ultimem_unhide

.code

; Returns ID.
.proc _ultimem_unhide
    lda $9f55
    lda $9faa
    lda $9f01
    lda $9ff3
    rts
.endproc
