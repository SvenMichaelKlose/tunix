; X/Y: First block of store.
;
; Returns:
; bs_found_block: Index of found block.
bs_alloc:
    ;; Map in first block of store to $2000.
    lda $9ff8
    pha     ; Save old bank.
    stx $9ff8
    sty $9ff9

    ;; Find free block in allocation map.
    lda bs_bam_start
    sta bs_bam_ptr
    lda @(++ bs_bam_start)
    sta @(++ bs_bam_ptr)
    ldy #0
l:  ldx (bs_bam_ptr),y
    cpx #$ff
    bne +f
    inc bs_bam_ptr
    bne +n
    inc @(++ bs_bam_ptr)
n:  lda bs_bam_ptr
    cmp bs_bam_end
    bne -l
    lda @(++ bs_bam_ptr)
    cmp @(++ bs_bam_end)
    bne -l
    lda bs_bam_start
    sta bs_bam_ptr
    lda @(++ bs_bam_start)
    sta @(++ bs_bam_ptr)
    jmp -l

f:  lda bs_bam_ptr
    sec
    sbc bs_bam_start
    sta bs_found_block
    lda @(++ bs_bam_tr)
    sbc @(++ bs_bam_start)
    sta @(++ bs_found_block)

    txa
l:  lsr
    bcs +l
    inc bs_found_block
    bne -l
    inc @(++ bs_found_block)
    jmp -l

r:  pla
    sta $9ff8
    lda #0
    sta $9ff9
    rts

; X/Y: First block of store.
: bs_block: Block index to free.
bs_free:
    ;; Map in first block of store to $2000.
    lda $9ff8
    pha     ; Save old bank.
    stx $9ff8
    sty $9ff9

    lda bs_block
    and #%00000111
    tax
    lsr @(++ bs_block)
    ror bs_block
    lsr @(++ bs_block)
    ror bs_block
    lsr @(++ bs_block)
    ror bs_block
    lda bs_block
    clc
    adc bs_bam_start
    sta bs_block
    lda @(++ bs_block)
    adc @(++ bs_bam_start)
    sta @(++ bs_block)
    ldy #0
    lda (bs_block),y
    and bitmasks,x
    sta (bs_block),y

    jmp -r
