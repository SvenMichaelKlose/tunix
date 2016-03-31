alloc_bank:
    jsr take_over

    txa
    pha
    tya
    pha
    lda $9ff4
    pha

    lda #0
    sta $9ff4

    ; Find something free in the bitmap of allocated banks.
mod_max_banks:
    ldx #@(-- (/ 1024 8 8))
l:  lda master_banks,x
    cmp #$ff
    bne found_bank
    dex
    cpx #$ff
    bne -l

    sec

    pla
    sta $9ff4

return:
    pla
    tay
    pla
    tax
    jmp release

    ; Calculate number of bank for bit 0.
found_bank:
    tay
    txa
    asl
    asl
    asl
    sta tmp
    tya

    ; Find free bank (unset bit).
    ldy #0
l:  lsr
    bcc +n
    inc tmp
    iny
    jmp -l

    ; Mark bank as allocated.
n:  lda bits,y
    ora master_banks,x
    sta master_banks,x

    pla
    sta $9ff4
    lda bits,y
    ora banks,x
    sta banks,x

    clc
    jmp -return

free_bank:
    jsr take_over

    ;; Save bank to free.
    tax

    ;; Get bit number in bank map.
    and #%00000111
    tay

    ;; Get byte index in bank map.
    txa
    lsr
    lsr
    lsr
    tax

    ;; Check if bank has been allocated.
    lda banks,x
    and bits,y
    beq err_not_allocated

    ;; Unset bit in map.
    lda banks,x
    and bitmasks,y
    sta banks,x

    ;; Switch to master core.
    lda $9ff4
    pha
    lda #0
    sta $9ff4

    ;; Unset bit in master core map.
    lda master_banks,x
    and bitmasks,y
    sta master_banks,x

    ;; Restore callee's core.
    pla
    sta $9ff4

    clc
    jmp release

err_not_allocated:
    sec
    jmp release

free_process_banks:
    ;; Save process' core.
    ldy $9ff4

    ;; Mask out bit in master core's bank map.
    ldx @(++ mod_max_banks)

    ; Get byte and invert it.
l:  lda banks,x
    eor #$ff

    ; Switch to master core.
    pha
    lda #0
    sta $9ff4
    pla

    ; Mask out bits.
    and master_banks,x
    sta master_banks,x

    ; Return to process' core.
    sty $9ff4

    dex
    bpl -l

    rts

bits:   1 2 4 8 16 32 64 128

bitmasks:
    %11111110
    %11111101
    %11111011
    %11110111
    %11101111
    %11011111
    %10111111
    %01111111
