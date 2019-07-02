.export ultimem_unhide_regs

.code

; Returns ID.
.proc ultimem_unhide_regs
    lda $9f55
    lda $9faa
    lda $9f01
    lda $9ff3
    rts
.endproc
