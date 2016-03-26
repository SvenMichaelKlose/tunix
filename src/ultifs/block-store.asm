; Returns:
; d: Index of found block.
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
    sta d
    lda @(++ d)
    sbc @(++ bs_bam_start)
    sta @(++ d)
    asl d
    rol @(++ d)
    asl d
    rol @(++ d)
    asl d
    rol @(++ d)

    txa
l:  lsr
    bcs +l
    inc d
    bne -l
    inc @(++ d)
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
