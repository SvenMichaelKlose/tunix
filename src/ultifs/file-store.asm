fs_create:
    jmp bs_alloc

fs_get_last_block:
    ldx #$ff
    sta c
    dex
    sta @(++ c)

; Get particular or last block.
; s: First block of file.
; c: Block index > 0.
fs_get_block:
    lda s
    ora @(++ s)
    beq +r

    inc @(++ c)

    ldy #0

    ; Check on last block (0).
l:  lda #2
    sta $9ff8
    lda (s),y
    inc $9ff8
    tax
    bne +n
    lda (s),y
    beq +f

    ; Step to next block.
n:  sta @(++ s)
    stx s

    ; Count down to wanted block.
    dec c
    bne -l
    dec @(++ c)
    bne -l

f:  rts

; s: First block of file.
fs_grow:
    jsr fs_get_last_block
    jsr bs_alloc
    lda #2
    sta $9ff8
    lda d
    ldy #0
    sta (s).y
    lda d
    ora #$20
    inc $9ff8
    sta (s),y
    rts

; Get particular or last block.
; s: First block of file.
; c: Block index > 0.
fs_shrink:
    jsr fs_get_block

; Get particular or last block.
fs_free:
    ; Free on block store.
    jsr bs_free

    ; Check on last block (0).
    ldy #0
    lda #2
    sta $9ff8
    lda (s),y
    inc $9ff8
    tax
    bne +n
    lda (s),y
    beq +f
n:

    ; Clear entry in FAM.
    pha
    lda #2
    sta $9ff8
    tya
    sta (s),y
    inc $9ff8
    sta (s),y

    ; Step to next block.
    pla
    sta @(++ s)
    stx s

    dec c
    bne fs_free
    dec @(++ c)
    bne fs_free

f:  rts
