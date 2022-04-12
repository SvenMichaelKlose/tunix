; UltiFS process-space wedge
;
; Banks in the secondary wedge.
;
; Author: Sven Michael Klose <pixel@hugbox.org>


.export _init_primary_wedge, unmap_ofs

.import uopen, uclose, uchkin, uckout, uclrcn
.import ubasin, ubsout, uclall, uload, usave


    .zeropage

s:      .res 2
v:      .res 2
d:      .res 2


    .data

FSTOP   = $f770
FGETIN  = $f1f5
BREAK   = $fed2
FCHROUT = $f27a

secondary_vectors_l:
    .byte <uopen, <uclose, <uchkin, <uckout, <uclrcn
    .byte <ubasin, <ubsout, <FSTOP, <FGETIN, <uclall, <BREAK, <uload, <usave, 0
secondary_vectors_h:
    .byte >uopen, >uclose, >uchkin, >uckout, >uclrcn
    .byte >ubasin, >ubsout, >FSTOP, >FGETIN, >uclall, >BREAK, >uload, >usave, 0


    .code

cpu_state       = $9c00
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

    ; Bump pointer past subroutines.
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
    ; Save accu and flags.
    sta cpu_state
    php
    pla
    sta cpu_state+3

    sei

    ; Save Ultimem status and BLK1.
    lda $9ff8
    sta cpu_state+5
    lda $9ff9
    sta cpu_state+6
    lda $9ff2
    sta cpu_state+4

    ; Bank in secondary wedge on BLK1.
    ora #%00000011  ; (R/W RAM for BLK1)
    sta $9ff2
    lda #117        ; TODO: Should be configurable.
    sta $9ff8
    lda #0
    sta $9ff9

    rts
map_ultimem_end:
.endproc

; Map process back in and set accu and flags.
.proc unmap_ultimem
    ; Restore Ultimem status and BLK1.
    lda cpu_state+4
    sta $9ff2
    lda cpu_state+5
    sta $9ff8
    lda cpu_state+6
    sta $9ff9

    ; Get back accu and flags.  X and Y register have
    ; been restored by the secondary wedge already.
    lda cpu_state+3
    pha
    lda cpu_state
    plp     ; (Will also restore the interrupt flag.)

    rts
.endproc

.proc unmap_ultimem_end
    nop
.endproc
