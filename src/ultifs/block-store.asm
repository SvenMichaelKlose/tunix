; Returns:
; s: Index of found block.
bs_alloc:
    ;; Find free block in allocation map.
    lda bs_bam_start
    sta d
    lda @(++ bs_bam_start)
    sta @(++ d)

    ldy #0
l:  ldx (d),y
    cpx #$ff
    bne +f
    inc d
    bne +n
    inc @(++ d)
n:  lda d
    cmp bs_bam_end
    bne -l
    lda @(++ d)
    cmp @(++ bs_bam_end)
    bne -l
    lda bs_bam_start
    sta d
    lda @(++ bs_bam_start)
    sta @(++ d)
    jmp -l

f:  lda d
    sec
    sbc bs_bam_start
    sta s
    lda @(++ d)
    sbc @(++ bs_bam_start)
    sta @(++ s)
    asl s
    rol @(++ s)
    asl s
    rol @(++ s)
    asl s
    rol @(++ s)

    txa
l:  lsr
    bcs +l
    inc s
    bne -l
    inc @(++ s)
    jmp -l

r:  pla
    sta $9ff8
    lda #0
    sta $9ff9
    rts

; d: Block index to free.
bs_free:
    lda #2
    sta $9ff8

    lda d
    and #%00000111
    tax
    lsr @(++ d)
    ror d
    lsr @(++ d)
    ror d
    lsr @(++ d)
    ror d
    lda d
    clc
    adc bs_bam_start
    sta d
    lda @(++ d)
    adc @(++ bs_bam_start)
    sta @(++ d)
    ldy #0
    lda (d),y
    and bitmasks,x
    sta (d),y

    jmp -r
