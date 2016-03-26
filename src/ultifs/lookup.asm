; s: Directory ID
find_dirent:
    lda d
    pha
    lda @(++ d)
    pha
    jsr fs_get_block
    jsr fs_map_block
    pla
    sta @(++ d)
    pla
    sta d

l:  ldy #0
    lda (s),y
    beq +err_not_found

    jsr compare_asciiz
    beq +f

    lda s
    clc
    adc #dirent_size
    sta s
    bcc -l
    inc @(++ s)
    jmp -l

f:  clc
    rts

; s: Directory ID
lookup:
    jsr find_dirent
    bcs +err_not_found

f:  ldy #dirent_id

    ; Copy file ID.
    lda (s),y
    sta d
    iny
    lda (s),y
    sta @(++ d)
    iny

    ; Copy entry type.
    lda (s),y
    sta c
    clc
    rts

err_not_found:
    sec
    rts
