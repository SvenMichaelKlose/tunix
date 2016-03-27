gopen:
    jsr take_over

    lda #2
    ldx #8
    ldy #0
    jsr setlfs

    ; Get length of file name and set it.
    ldy #0
l:  lda (s),y
    beq +n
    iny
    jmp -l
n:  tya
    ldx s
    ldy @(++ s)
    jsr setnam

    jsr open
    bcs +error

    ldx #2
    jsr chkin

    php
    jsr release
    plp
    rts

error:
    jsr release
    sec
    rts

read:
    jsr take_over

    jsr readst
    bne +eof

    jsr chrin
    pha
    lda $90
    cmp #1   ; set carry when ST>0 (i.e., <>0!)
    pla      ; keep carry, and possibly set Z flag for byte=0
    jmp release
eof:
    jsr release
    sec
    rts


    ; Get size of block.
readw:
    jsr read
    bcs +e
    sta c
    jsr read
    bcs +e
    sta @(++ c)
    rts

readm:
    jsr readw

readn:
    inc @(++ c)

l:  jsr read
    bcs +done

    ldy #0
    sta (d),y
    jsr inc_d

    ; Decrement counter and check if done.
    dec c
    bne -l
    dec @(++ c)
    bne -l

done:
    ldy #0
    sta (d),y
    clc
    rts

e:  sec
    rts

error:
gclose:
    jsr take_over
    jsr clrchn
    lda #2
    jsr close
    jmp release
