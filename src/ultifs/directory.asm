; s: Directory ID
; d: File name.
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

get_free_dirent:
    lda d
    pha
    lda @(++ d)
    pha
    jsr fs_get_block
    jsr fs_map_block
    lda #<zero
    sta d
    lda #>zero
    sta @(++ d)
    jsr scan_dirent
    pla
    sta @(++ d)
    pla
    sta d
    rts

; s: Directory ID
; d: New directory entry.
alloc_dirent:
    jsr get_free_dirent
    ldy #@(-- dirent_size)
l:  lda (d),y
    sta (s),y
    dey
    bpl -l
    rts

; s: Directory ID
; d: File name.
free_dirent:
    jsr scan_dirent
    lda s
    clc
    adc #dirent_size
    sta d
    lda @(++ s)
    adc #0
    sta @(++ d)

m:  lda (d),y
    beq +n
    ldy #@(-- dirent_size)
l:  lda (d),y
    sta (s),y
    jsr inc_s
    jsr inc_d
    dey
    bpl -l
    jmp -m

n:  sta (s),y
    rts
