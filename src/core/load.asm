setlf   = $ffba
setnam  = $ffbd
open    = $ffc0
close   = $ffc3
chkin   = $ffc6
clrchn  = $ffcc
chrin   = $ffcf
clall   = $ffe7

gopen:
    lda #2
    ldx #8
    ldy #0
    jsr setlf

    ; Get length of file name and set it.
    ldy #0
l:  lda (s),y
    beq +n
    iny
    jmp -l
n:  tya
    ldx #<s
    ldy #>s
    jsr setnam

    jsr open
    bcs +error

    ldx #2
    jmp chkin

error:
    sec
    rts

gchrin:
    jsr $ffb7
    bne +eof

    jsr chrin
    pha
    lda $90
    cmp #1   ; set carry when ST>0 (i.e., <>0!)
    pla      ; keep carry, and possibly set Z flag for byte=0
    rts
eof:
    sec
    rts

gload:
    ; Get size of block.
    jsr gchrin
    bcs +e
    sta c
    jsr gchrin
    bcs +e
    tay
    dey
    sty @(++ c)

l:  jsr gchrin
    bcs +done

    ldy #0
    sta (d),y

    ; Step to next destination address.
    inc d
    bcc +n
    inc @(++ d)
n:

    ; Decrement counter and check if done.
    dec c
    bne -l
    dec @(++ c)
    bne -l

done:
    clc
    rts

e:  sec
    rts

error:
gclose:
    jsr clrchn
    lda #2
    jmp close
