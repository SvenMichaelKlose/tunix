.export init_alloc, alloc_bank, free_bank

num_banks = 1024 / 8    ; TODO: Use detection for VIC-MIDI.

.data

ram_map:            .res 16
last_bank_in_map:   .res 0
free_banks:         .res 1

.code

alloc_masks:
    .byte %00000001
    .byte %00000010
    .byte %00000100
    .byte %00001000
    .byte %00010000
    .byte %00100000
    .byte %01000000
    .byte %10000000

free_masks:
    .byte %11111110
    .byte %11111101
    .byte %11111011
    .byte %11110111
    .byte %11101111
    .byte %11011111
    .byte %10111111
    .byte %01111111

.proc init_alloc
    lda #%01000011
    sta $9ff2
    lda #$ff
    sta $9ff8
    sta $9ff9

    ldx #num_banks/8
    lda #$7f
    sta ram_map-1,x
    dex
    lda #$ff
l1: sta ram_map-1,x
    dex
    bne l1

    lda #num_banks-1
    sta free_banks
    lda #0
    sta last_bank_in_map
    rts
.endproc

.proc alloc_bank
    lda $9ff8
    pha
    lda $9ff9
    pha
    lda $9ff2
    pha
    and #%11111100
    ora #%00000001
    sta $9ff2
    lda #$ff
    sta $9ff8
    sta $9ff9

    ldx free_banks
    beq all_gone

    ldx last_bank_in_map
again:
    lda ram_map,x
    bne found
    inx
    bpl again
    ldx #0
    bne again       ; (jmp)

found:
    ldy #0
next_bit:
    lsr
    bcs got_it
    iny
    bne next_bit    ; (jmp)
    
got_it:
    lda ram_map,x
    and alloc_masks,y
    sta ram_map,x
    txa
    asl
    asl
    asl
    sty $33c
    dec free_banks  ; (Clear carry, too.)
    adc $33c
    sta $33c
return:
    pla
    sta $9ff9
    pla
    sta $9ff8
    pla
    sta $9ff1
    lda $33c
    rts

all_gone:
    sec
    bcs return      ; (jmp)
.endproc

.proc free_bank
    tax
    lda $9ff8
    pha
    lda $9ff9
    pha
    lda $9ff2
    pha
    and #%11111100
    ora #%00000001
    sta $9ff2
    lda #$ff
    sta $9ff8
    sta $9ff9

    txa
    and #%00000111
    tay
    txa
    lsr
    lsr
    lsr
    tax
    lda ram_map,x
    and alloc_masks,y
    bne error
    lda free_masks,y
    ora ram_map,x
    sta ram_map,x

    clc
return:
    pla
    sta $9ff9
    pla
    sta $9ff8
    pla
    sta $9ff1
    rts

error:
    sec
    bcs return
.endproc
