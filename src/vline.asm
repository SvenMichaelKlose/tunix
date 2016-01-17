vmaskd: %01111111
        %10111111
        %11011111
        %11101111
        %11110111
        %11111011
        %11111101
        %11111110

vmasks: %10000000
        %01000000
        %00100000
        %00010000
        %00001000
        %00000100
        %00000010
        %00000001

done:
    pla
    sta height
    pla
    sta ypos
    rts

; SYSCALL: Draw a vertical line
;
; In: xpos, ypos, height, pattern
; Modifies: masks, maskd
vline:
    lda ypos
    pha
    clc
    adc height
    sta ypos2
    lda height
    pha

    ; Clip
    lda xpos
    cmp rxl
    bcc -done
    cmp rxr
    bcs -done

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

    lda xpos
    and #7
    tax
    lda vmasks,x
    sta masks
    lda vmaskd,x
    sta maskd

    jsr vfill
    jmp -done
