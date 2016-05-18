.export vline

.importzp xpos, ypos, height, rxl, rxr, masks, maskd
.import clip_y, vfill, calcscr, add_region_position, sub_region_position, dec_scr

.code

; SYSCALL: Draw a vertical line
;
; In: xpos, ypos, height, pattern
; Modifies: masks, maskd
.proc vline
    jsr add_region_position
    lda ypos
    pha
    lda height
    pha

    ; Clip
    lda xpos
    cmp rxl
    bcc done
    cmp rxr
    beq +n
    bcs done
n:  jsr clip_y
    bcc done

    lda xpos
    and #7
    tax
    lda vmasks,x
    sta masks
    lda vmaskd,x
    sta maskd

    jsr calcscr
    jsr dec_scr
    jsr vfill

done:
    pla
    sta height
    pla
    sta ypos
    jmp sub_region_position
.endproc

.data

vmaskd:
    .byte %01111111
    .byte %10111111
    .byte %11011111
    .byte %11101111
    .byte %11110111
    .byte %11111011
    .byte %11111101
    .byte %11111110

vmasks:
    .byte %10000000
    .byte %01000000
    .byte %00100000
    .byte %00010000
    .byte %00001000
    .byte %00000100
    .byte %00000010
    .byte %00000001
