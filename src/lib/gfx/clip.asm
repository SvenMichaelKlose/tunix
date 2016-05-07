.export clip, clip_x, clip_y
.import xpos, xpos2, width, rxl, rxr
.code

clip_x:
    ldx #0

clip:
    lda xpos,x
    clc
    adc width,x
    sta xpos2,x

    lda xpos,x
    cmp rxl,x
    bcs n
    lda rxl,x
    sta xpos,x
n:

    lda xpos2,x
    cmp rxr,x
    bcc m
    lda rxr,x
    sta xpos2,x
m:

    lda xpos2,x
    sec
    sbc xpos,x
    bcc done
    sta width,x

    sec
done:
    rts

clip_y:
    ldx #1
    bne clip    ; (jmp)
