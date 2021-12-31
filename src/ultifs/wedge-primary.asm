; Process-space wedge
;
; Banks in the secondary wedge.

.export _init_primary_wedge, unmap_ofs

.import uopen, uclose, uchkin, uckout, uclrcn, ubasin, ubsout, uclall


.zeropage

s:      .res 2
v:      .res 2
d:      .res 2


.data

secondary_vectors_l:
    .byte <uopen, <uclose, <uchkin, <uckout, <uclrcn, <ubasin, <ubsout, <uclall, 0
secondary_vectors_h:
    .byte >uopen, >uclose, >uchkin, >uckout, >uclrcn, >ubasin, >ubsout, >uclall, 0


.code

kernal_vectors  = $031a
wedge_start     = $9800
unmap_ofs       = unmap_ultimem - map_ultimem

; A: Wedge start low byte.
; X: Wedge start high byte.
.proc _init_primary_wedge
    ; Pointer to start of wedge.
    sta d
    stx d+1

    ; Copy wedge's subroutines.
    ldy #unmap_ultimem_end - map_ultimem
l1: lda map_ultimem,y
    sta (d),y
    dey
    bpl l1

    lda #unmap_ultimem_end - map_ultimem
    clc
    adc d
    sta d
    lda d+1
    adc #0
    sta d+1

    ;;; Create trampolines.
    ldx #0
l2: lda secondary_vectors_l,x
    ora secondary_vectors_h,x
    beq done

    ; Point vector to the code we're about to generate.
    txa
    asl
    tay
    lda d
    sta kernal_vectors,y
    lda d+1
    sta kernal_vectors+1,y

    ; Generate "JSR map_ultimem".
    lda #$20
    jsr out
    lda #<wedge_start
    jsr out
    lda #>wedge_start
    jsr out

    ; Generate "JMP <secondary wedge>".
    lda #$4c
    jsr out
    lda secondary_vectors_l,x
    jsr out
    lda secondary_vectors_h,x
    jsr out

    inx
    jmp l2

done:
    rts

out:ldy #0
    sta (d),y
    inc d
    bne r
    inc d+1
r:  rts
.endproc

; Map in secondary wedge and save registers.
.proc map_ultimem
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
    lda #117
    sta $9ff8
    lda #0
    sta $9ff9

    rts
map_ultimem_end:
.endproc

; Map process back in and set accu and flags.
.proc unmap_ultimem
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
.endproc

.proc unmap_ultimem_end
    nop
.endproc
