clip_x:
    lda xpos
    clc
    adc width
    sta xpos2

    lda xpos
    cmp rxl
    bcs +n
    lda rxl
    sta xpos
n:

    lda xpos2
    cmp rxr
    bcc +n
    lda rxr
    sta xpos2
n:

    lda xpos2
    sec
    sbc xpos
    bcc +done
    sta width

    lda ypos
    clc
    adc height
    sta ypos2
    sec
done:
    rts

clip_y:
    lda ypos
    clc
    adc height
    sta ypos2

    lda ypos
    cmp ryt
    bcs +n
    lda ryt
    sta ypos
n:

    lda ypos2
    cmp ryb
    bcc +n
    lda ryb
    sta ypos2
n:

    lda ypos2
    sec
    sbc ypos
    bcc -done
    sta height
    sec
    rts
