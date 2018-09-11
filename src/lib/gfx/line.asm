; Based on VIC-20 TGI driver done for cc65.

.export     line

.importzp   xpos, xpos2, ypos, ypos2, scr, c
.import     column_addrs_lo, column_addrs_hi
.import     calcscr

YRES    = 12 * 16

    .data

dx:         .res 2
dy:         .res 1
chunk:      .res 1
oldchunk:   .res 1
bitmask:    .res 1
tmp:        .res 1
tmp2:       .res 1

    .rodata

bittab:     .byte   $80,$40,$20,$10,$08,$04,$02,$01
bitchunk:   .byte   $FF,$7F,$3F,$1F,$0F,$07,$03,$01

    .code

.proc line
        lda #$ff
        sta bitmask

l1:     lda     xpos2           ;Make sure x1<x2
        sec
        sbc     xpos
        tax
        lda     xpos2+1
        sbc     xpos+1
        bcs     l2
        lda     ypos2           ;If not, swap P1 and P2
        ldy     ypos
        sta     ypos
        sty     ypos2
        lda     ypos2+1
        ldy     ypos+1
        sta     ypos+1
        sty     ypos2+1
        lda     xpos
        ldy     xpos2
        sty     xpos
        sta     xpos2
        lda     xpos2+1
        ldy     xpos+1
        sta     xpos+1
        sty     xpos2+1
        bcc     l1          ; (jmp)

l2:     sta     dx+1
        stx     dx

        ldx     #$c8         ;INY
        lda     ypos2           ;Calculate dy
        sec
        sbc     ypos
        tay
        lda     ypos2+1
        sbc     ypos+1
        bpl     l3             ;Is y2>=y1?
        lda     ypos           ;Otherwise dy=y1-y2
        sec
        sbc     ypos2
        tay
        ldx     #$88         ;DEY

l3:     sty     dy              ; 8-bit dy -- FIX ME?
        stx     yincdec
        stx     xincdec

        jsr     calcscr
        ldy     ypos

        lda     xpos
        and     #7
        tax
        lda     bitchunk,X
        sta     oldchunk
        sta     chunk

        ldx     dy
        cpx     dx           ;Who's bigger: dy or dx?
        bcc     stepinx      ;If dx, then...
        lda     dx+1
        bne     stepinx

;
; Big steps in Y
;
;   X is now counter, Y is y-coordinate
;
; On entry, X=dy=number of loop iterations, and Y=ypos
stepiny:
        lda     #00
        sta     oldchunk     ;So plotting routine will work right
        lda     chunk
        lsr                  ;Strip the bit
        eor     chunk
        sta     chunk
        txa
        bne     l4        ;If dy=0 it's just a point
        inx
l4:     lsr                  ;Init counter to dy/2
;
; Main loop
;
yloop:  sta     tmp

        lda     (scr),y    ;Otherwise plot
        eor     bitmask
        and     chunk
        eor     (scr),y
        sta     (scr),y
yincdec:
        iny                  ;Advance Y coordinate
        lda     tmp         ;Restore A
        sec
        sbc     dx
        bcc     yfixx
ycont:  dex                  ;X is counter
        bne     yloop
ycont2: lda     (scr),y    ;Plot endpoint
        eor     bitmask
        and     chunk
        eor     (scr),y
        sta     (scr),y
        rts

yfixx:                      ;x=x+1
        adc     dy
        lsr     chunk
        bne     ycont        ;If we pass a column boundary...
        ror     chunk        ;then reset chunk to $80
        sta     tmp2
        lda     scr
        adc     #YRES
        sta     scr
        bcc     l5
        inc     scr+1
l5:     lda     tmp2
        dex
        bne     yloop
        beq     ycont2

;
; Big steps in X direction
;
; On entry, X=dy=number of loop iterations, and Y=ypos

stepinx:
        ldx     dx
        lda     dx+1
        sta     c
        cmp     #$80
        ror                  ;Need bit for initialization
        sta     ypos         ;High byte of counter
        txa
        bne     @CONT        ;Could be $100
        dec     c
@CONT:  ror
;
; Main loop
;
xloop:  lsr     chunk
        beq     xfixc        ;If we pass a column boundary...
xcont1: sbc     dy
        bcc     xfixy        ;Time to step in Y?
xcont2: dex
        bne     xloop
        dec     c      ;High bits set?
        bpl     xloop

        lsr     chunk        ;Advance to last point
        jmp     plot     ;Plot the last chunk
;
; chunk has passed a column, so plot and increment pointer
; and fix up chunk, oldchunk.
;
xfixc:  sta     tmp
        jsr     plot
        lda     #$FF
        sta     chunk
        sta     oldchunk
        lda     scr
        clc
        adc     #YRES
        sta     scr
        lda     tmp
        bcc     xcont1
        inc     scr+1
        jmp     xcont1
;
; Check to make sure there isn't a high bit, plot chunk,
; and update Y-coordinate.
;
xfixy:  dec     ypos           ;Maybe high bit set
        bpl     xcont2
        adc     dx
        sta     tmp
        lda     dx+1
        adc     #$FF         ;Hi byte
        sta     ypos

        jsr     plot     ;Plot chunk
        lda     chunk
        sta     oldchunk

        lda     tmp
xincdec:
        iny                  ;Y-coord
        jmp     xcont2

;
; Subroutine to plot chunks/points (to save a little
; room, gray hair, etc.)
;
plot:                       ; Plot the line chunk
        lda     (scr),Y       ; Otherwise plot
        eor     bitmask
        ora     chunk
        and     oldchunk
        eor     chunk
        eor     (scr),Y
        sta     (scr),Y
        rts
.endproc
