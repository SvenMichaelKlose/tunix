__VIC20__ = 1

.include "cbm_kernal.inc"

.export printstr

.importzp printptr


    .code

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
