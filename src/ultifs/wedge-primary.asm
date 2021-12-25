; Process-space wedge
;
; Banks in the secondary wedge.

.export _init_primary_wedge
.exportzp unmap

.import uopen, uclose, uchkin, uckout, uclrcn, ubasin, ubsout, uclall


.zeropage

s:      .res 2
v:      .res 2
d:      .res 2
unmap:  .res 2


.data

secondary_vectors_l:
    .byte <uopen, <uclose, <uchkin, <uckout, <uclrcn, <ubasin, <ubsout, <uclall, 0
secondary_vectors_h:
    .byte >uopen, >uclose, >uchkin, >uckout, >uclrcn, >ubasin, >ubsout, >uclall, 0


.code

kernal_vectors  = $031a
wedge_start     = $9800

.proc _init_primary_wedge

map_size        = unmap_ultimem_end - map_ultimem
map_ofs         = 0
unmap_ofs       = unmap_ultimem - map_ultimem

    ; Pointer to start of wedge.
    lda #<wedge_start
    sta d
    lda #>wedge_start
    sta d+1

    ; Copy wedge's subroutines.
    ldy #map_size
l1: lda map_ultimem,y
    sta (d),y
    dey
    bpl l1

    ; Calculate address of unmap_ultimem.
    lda #<wedge_start
    clc
    adc #<unmap_ofs
    sta unmap
    lda #>kernal_vectors
    adc #>wedge_start
    sta unmap+1

    ; Create trampolines.
    ldx #0
    ldy #map_size
l2: lda secondary_vectors_l,x
    ora secondary_vectors_h,x
    beq done

    ; JSR map_ultimem
    lda #$20
    jsr out
    lda #<wedge_start
    jsr out
    lda #>wedge_start
    jsr out

    ; JMP <secondary wedge>
    lda #$4c
    jsr out
    lda secondary_vectors_l,x
    jsr out
    lda secondary_vectors_h,x
    jsr out

    ; TODO: Set vector in lowmem.
    inx
    jmp l2

done:
    rts

out:sta (d),y
    iny
    rts

outv:
    tya
    pha
    ldy #0
    sta (v),y
    inc v
    bne n
    inc v+1
n:  pla
    tay
    rts

; Map in secondary wedge and save registers.
map_ultimem:
    ; Save registers to stack page.
    sta $100
    stx $101
    sty $102

    ; Save Ultimem status.
    lda $9ff8
    sta $104
    lda $9ff9
    sta $105
    lda $9ff2
    sta $106

    ; Bank in secondary wedge on BLK1.
    ora #%11000000  ; (RAM)
    sta $9ff2
    lda #8
    sta $9ff8
    lda #0
    sta $9ff9

    rts
map_ultimem_end:

; Map process back in and set accu and flags.
unmap_ultimem:
    lda $104
    sta $9ff8
    lda $105
    sta $9ff9
    lda $106
    sta $9ff2

    lda $103
    pha
    lda $100
    plp
    rts
unmap_ultimem_end:
.endproc
