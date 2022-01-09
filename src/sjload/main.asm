; SJLOAD/SJSAVE 07 - VIC-20 Version
;
; VIC-20 floppy speeder and disk utility based on VDOS for the
; Commodore C64 by Edward Carroll in 1986.
;
; Modifications:
;
;   Jiffy compatibility added by 1570 in 2008.
;   VIC-20 compatibility and support for  Jiffy save by diddl in 2009.
;   KERNAL wedge added by diddl in 2010.
;   v07 version for vic-20 by nbla000 in 2010:
;       - enabled saving/loading kernal messages when PRINTADDRESS = 0 (if PRINTMESSAGE = 1)
;       - bug fix, vic hangs when a drive not present was used
;       - bug fix, drive not closed when load is finished
;       - removed unused routines to reduce program size (few bytes) 
;       - added PRINTMESSAGE, LOADPARAMS and SAVEPARAMS compilation parameters
;
;   Cleaned up ca65 version by Sven Michael Klose <pixel@hugbox.org> in 2022.

.export sjload_init

SJLOAD_START  = $9800

STANDALONE      = 1 ; Make LOADable stand-alone binary.
SJLOAD          = 1 ; Extend LOAD.
LOADPARAMS      = 1 ; Support additional BASIC LOAD parameters (destination address).
SJSAVE          = 1 ; Extend SAVE.
SAVEPARAMS      = 1 ; Support additional BASIC SAVE parameters (source address, length).
FULLKERNAL      = 1 ; Extend all other KERNAL I/O functions.
PRINTADDRESS    = 1 ; LOAD/SAVE: Print start/end address
PRINTMESSAGE    = 0 ; When PRINTADDRESS == 0: Print saving/loading message like standard kernal routines.

    .code

.ifdef STANDALONE

    .org SJLOAD_START - 2
    .byte <SJLOAD_START, >SJLOAD_START

.endif

ptr         = $14

CHRGET      = $73   ;get next char
CHRGOT      = $79   ;get last char

STATUS      = $90
VERCK       = $93
LDTND       = $98
DFLTN       = $99
DFLTO       = $9a
MSGFLG      = $9d   ; Use to check if RUNning a program.
                    ; TODO: Test if input line is at $0200.
SAL         = $ac
EAL         = $ae
LFN         = $b7
LA          = $b8
SA          = $b9
FA          = $ba
FNADR       = $bb

STAL        = $c1
MEMUSS      = $c3

ptr_chkin   = $031e
ptr_chkout  = $0320
ptr_clrch   = $0322
ptr_basin   = $0324
ptr_basout  = $0326
ptr_getin   = $032a
ptr_clrall  = $032c
ptr_load    = $0330
ptr_save    = $0332

; ROM routines
PRTSTR      = $cb1e ; Print string in YA.
TYPCHK      = $cd8a ; get numeric value
MAKADR      = $d7f7 ; convert to word value into y/a; $14 (ptr)
SCATN       = $eec5
BSOUT       = $ffd2
STOP        = $ffe1 ; check stop key
SEROUT1     = $e4a0
RSPAUSE     = $f160
SRCLKHI     = $ef84
SRCLKLO     = $ef8d ; Set serial clock low.
WAITABIT    = $ef96
SERGET      = $e4b2
SEROUT0     = $e4a9
SRBAD       = $eeb4 ; Error: DEVICE NOT PRESENT
ORIOST      = $fe6a ; Set STATUS.
FLOAD2      = $f549
SRCHING     = $f647 ; Print SEARCHING.
FE_NTFND    = $f787 ; Error FILE NOT FOUND.
LDVRMSG     = $f66a ; Print LOADING / VERIFYING.
FSAVE2      = $f685
RD300       = $fbd2 ; $c1/$c2 -> $ac/$ad
SAVING      = $f728
VPRTY       = $fd11
WRT62       = $fd1b ; incr addr
FE_DVNTP    = $f78a ; Error DEVICE NOT PRESENT.
FNDFLNO     = $f3cf ; Search logical file number.
FE_NTOPN    = $f784 ; Error FILE NOT OPEN.
SETFLCH     = $f3df ; Set file parametres.
FGETIN      = $f1f5
FCHRIN      = $f20e
FCHROUT_2   = $f27b

; ROM routines without "official" names.
WAIT_DEV_RECEIVED   = $eea0
SERIAL_BUS_TIMEOUT  = $eeb7
FACPTR_2        = $ef1c
SRBAD_2         = $eeb9 ; Set STATUS, unlisten.
WAIT_BUS_END    = $eed3
FLOAD2_2        = $f56d
FLOAD2_3        = $f58a
FSAVE2_2        = $f6cb ; Unlisten, close, break.
LDA0SEC         = $f6ce
CHK_STOP        = $f755 ; Test stop key.
FCHKIN_2        = $f2d2
FCHKIN_3        = $f2f8
FCHKIN_4        = $f301
FCHKOUT_2       = $f314
FCHKOUT_3       = $f33a
FCHKOUT_4       = $f342
CHRINSR_2       = $f268
FCLRCHN_2       = $f403

sjload_init:
    lda #<jload
    ldx #>jload
    sta ptr_load
    stx ptr_load + 1

.ifdef SJSAVE
    lda #<jsave
    ldx #>jsave
    sta ptr_save
    stx ptr_save + 1
.endif

.ifdef FULLKERNAL
    lda #<jchkin
    ldx #>jchkin
    sta ptr_chkin
    stx ptr_chkin + 1
    lda #<jchkout
    ldx #>jchkout
    sta ptr_chkout
    stx ptr_chkout + 1
    lda #<jbasin
    ldx #>jbasin
    sta ptr_basin
    stx ptr_basin + 1
    lda #<jbasout
    ldx #>jbasout
    sta ptr_basout
    stx ptr_basout + 1
    lda #<jgetin
    ldx #>jgetin
    sta ptr_getin
    stx ptr_getin + 1
    lda #<jclrch
    ldx #>jclrch
    sta ptr_clrch
    stx ptr_clrch + 1
    lda #<jclrall
    ldx #>jclrall
    sta ptr_clrall
    stx ptr_clrall + 1
.endif

  rts

jtalk:
    ora #$40
    .byte $2c       ; (bit absolute)

jlisten:
    ora #$20
    jsr RSPAUSE

jlisten_without_pause:
    pha
    bit $94
    bpl l6e2b
    sec
    ror $a3
    jsr new_iecout
    lsr $94
    lsr $a3
l6e2b:
    pla
    sta $95

    sei
    lda #$00
    sta $a3
    jsr SEROUT1

    cmp #$3f
    bne l6e38
    jsr SRCLKHI
l6e38:
    lda $911f
    ora #$80
    sta $911f

lee40:
    jsr SRCLKLO
    jsr SEROUT1
    jsr WAITABIT

old_iecout:
    sei
    jsr SEROUT1
    jsr SERGET
    lsr
    bcs l6eb4       ; Error DEVICE NOT PRESENT.

    jsr SRCLKHI
    bit $a3
    bpl l6e66
l6e5a:
    jsr SERGET
    lsr
    bcc l6e5a
l6e60:
    jsr SERGET
    lsr
    bcs l6e60
l6e66:
    jsr SERGET
    lsr
    bcc l6e66
    jsr SRCLKLO

    txa
    pha
    ldx #$08

l6e73:
    lda $911f
    and #$02
    bne l6e7f
    pla
    tax
    jmp SERIAL_BUS_TIMEOUT

l6e7f:
    jsr SEROUT1
    ror $95
    bcs l6e89
    jsr SEROUT0
l6e89:
    jsr SRCLKHI
    lda $912c
    and #$dd
    ora #$02
    php
    pha
    jsr lf96e
    pla
    plp
    dex
    bne l6e73

    pla
    tax
    jmp WAIT_DEV_RECEIVED

l6eb4:
    jmp SRBAD

l6eb7:
    jmp SERIAL_BUS_TIMEOUT

lf96e:
    sta $912c
    bit $911f
    bpl lf997
    cpx #$02
    bne lf997
    lda #$02
    ldx #$20
lf97e:
    bit $911f
    beq lf988
    dex
    bne lf97e
    beq lf995   ; (jmp)
lf988:
    bit $911f
    beq lf988
    lda $95
    ror
    ror
    ora #$40
    sta $a3
lf995:
    ldx #$02
lf997:
    rts

jiecin:
    sei
    bit $a3
    bvs jiffy_in
    lda #$00        ; TODO: jmp to $ef1a as there is a lda #0 already.
    jmp FACPTR_2    ;orig byte in

jiffy_in:
    lda $911f
    and #$03
    beq jiffy_in
    lda #$80
    sta $9c
    txa
    pha
    pha
    pla
    pha
    pla
    lda $912c
    and #$dd
    sta $912c
    ora #$20
    tax
    bit $9c
    bit $9c
    bit $9c
    lda $911f
    ror
    ror
    nop
    and #$80
    ora $911f
    rol
    rol
    sta $b3
    lda $911f
    ror
    ror
    and #$80
    nop
    ora $911f
    rol
    rol
    sta $c0
    lda $911f
    stx $912c
    sta $9c
    jsr b3c0_to_byte_in_accu

    sta $a4
    pla
    tax
    lda $9c
    ror
    ror
    bpl l7c54
    bcc lfc4f
    lda #$42
    jmp SRBAD_2     ; Set STATUS, unlisten.

jiecout:
    bit $94
    bmi leeed
    sec
    ror $94
    bne leef2
leeed:
    pha
    jsr new_iecout
    pla
leef2:
    sta $95
    clc
    rts

new_iecout:
    sei
    bit $a3
    bvs jiffy_out
    lda $a3
    cmp #$a0
    bcs jiffy_out
    jmp old_iecout

lfc4f:
    lda #$40
    jsr ORIOST

l7c54:
    lda $a4

l7c56:
    cli
    clc
    rts

jiffy_out:
    txa
    pha
    lda $95
    lsr
    lsr
    lsr
    lsr
    tax
    lda jouttab1,x
    pha
    txa
    lsr
    lsr
    tax
    lda jouttab1,x
    sta $b3
    lda $95
    and #$0f
    tax
    lda #$02
l7c76:
    bit $911f
    beq l7c76

    lda $912c
    and #$dd
    sta $9c
    pha
    pla
    pha
    pla
    nop
    nop
    nop
    sta $912c
    pla
    ora $9c
    nop
    sta $912c
    lda $b3
    ora $9c
    ora $9c
    sta $912c
    lda jouttab2,x
    ora $9c
    nop
    sta $912c
    lda jouttab2,x
    ora $9c
    nop
    sta $912c
    nop
    and #$dd
    bit $a3
    bmi l7cb7
    ora #$02
l7cb7:
    sta $912c
    pla
    tax
    nop
    lda $9c
    ora #$02
    sta $912c
    lda $911f
    and #$02
    beq l7c56
    jmp SERIAL_BUS_TIMEOUT

    .rodata

jouttab1:  .byte $00, $02, $20, $22, $00, $02, $20, $22, $00, $02, $20, $22, $00, $02, $20, $22
jouttab2:  .byte $00, $00, $20, $20, $00, $00, $20, $20, $02, $02, $22, $22, $02, $02, $22, $22

    .code

b3c0_to_byte_in_accu:
    lda $b3
    and #$0f
    sta $b3
    lda $c0
    asl
    asl
    asl
    asl
    ora $b3
    rts

juntalk:
    lda $911f
    ora #$80        ; Issue ATN.
    sta $911f
    jsr SRCLKLO
    lda #$5f
    .byte $2c       ; bit absolute

junlisten:
    lda #$3f
    jsr jlisten_without_pause
    jsr SCATN
    txa
    ldx #$0b
@wait:
    dex
    bne @wait
    tax
    jsr SRCLKHI
    jmp SEROUT1

jtalksa:
    sta $95
    jsr lee40
    jmp WAIT_BUS_END

jlistensa:
    sta $95
    jsr lee40
    jmp SCATN

; :: "fnam",pa,sa[,loadadr]
jload:
    ldx FA
    cpx #4
    bcs @dont_verify
    jmp FLOAD2

@dont_verify:
    sta VERCK
    lda #0
    sta LA

.ifndef PRINTADDRESS 
.ifdef PRINTMESSAGE
    jsr SRCHING ;print "searching" 
.endif
.endif

    lda #$f0        ; channel
    jsr disk_listen
    jsr iecnamout
    bcc l00a
    rts

l00a:
    ldy #$00
    lda (FNADR),y
    cmp #'$'
    bne l00b        ;directory? -->
    jmp FLOAD2_2    ;normal load

l00b:
    lda #$60
    jsr disk_talk

    jsr jiecin
    sta EAL

    lda STATUS
    lsr
    lsr
    bcc l00c
    jmp FE_NTFND

l00c:
    jsr jiecin
    sta EAL + 1

    ldx SA
.ifdef LOADPARAMS
    beq l00         ; SA=0 -->
    dex
    dex
.endif
    bne l01         ; SA=1 -->

.ifdef LOADPARAMS
l02:pha             ; SA=2: load cartridge at $c3
    lda EAL
    pha             ; first two bytes ...
.endif

l00:                ; SA=0: load program at $c3
.ifdef LOADPARAMS
    jsr frmword2    ; get word value
.endif

    lda MEMUSS + 1
    ldx MEMUSS
.ifdef LOADPARAMS
    bcs l2
    lda ptr + 1
    ldx ptr
l2:
.endif

    sta EAL + 1
    stx EAL

l01:
.ifdef PRINTADDRESS
    jsr print_atadr
.else
.ifdef PRINTMESSAGE
    jsr LDVRMSG     ;print "loading / verifying"  
.endif
.endif

    ldx SA

.ifdef LOADPARAMS
    dex
    dex
    bne l3          ; SA != 2 -->

    ldy #0
    pla
    jsr storebyte
    pla
    jsr storebyte

l3:
.endif

fastload:
    bit $a3
    bvs lfb1f       ; jiffy -->

    jsr FLOAD2_3
    bcc @n
    jmp FSAVE2_2    ; Unlisten, close, break.
@n:

mylo_e:
.ifdef PRINTADDRESS
    jsr print_toadr
.endif

    jsr disk_close_lo

    clc
    ldx EAL
    ldy EAL + 1
    rts

lfb1f:
    jsr juntalk
    lda #$61
    jsr disk_talk
    sei
    lda $b2
    pha
    ldy #$00

lfb25:
    jsr CHK_STOP
    cmp #$fe
    beq lfb5b
    lda $912c
    and #$dd
    tax
    ora #$20
    sta $b2
    stx $912c
    lda #$80
    sta $9c
lfb3d:
    lda $911f
    lsr
    bcc lfb3d
    and #$01
    beq lfb67
    ldx #$6d
lfb49:
    bit $911f
    beq lfb54
    dex
    bne lfb49
    lda #$42
    .byte $2c       ; bit abs
lfb54:
    lda #$40
    jsr ORIOST
    clc
    .byte $24       ; bit zp
lfb5b:
    sec
    pla
    sta $b2
    bcc mylo_e
lerr:
    jmp FSAVE2_2    ; Unlisten, close, break.

lfb67:
    lda #$02
lfb69:
    bit $911f
    beq lfb69
lfb6e:
    pha
    pla
    nop
    lda $b2
    sta $912c
    lda #$01
    bit $911f
    beq lfb25
    stx $912c
    lda $911f
    ror
    ror
    nop
    and #$80
    ora $911f
    rol
    rol
    nop
    sta $b3
    lda $911f
    ror
    ror
    and $9c
    ora $911f
    rol
    rol
    sta $c0

    lda #>(lfb6e - 1)   ; Push return address.
    pha
    lda #<(lfb6e - 1)
    pha
    jsr b3c0_to_byte_in_accu

storebyte:
    cpy $93
    bne verifybyte
    sta ($ae),y
lfba7:
    inc $ae
    bne @n
    inc $af
@n: rts

verifybyte:
    cmp ($ae),y
    beq lfba7
    lda #$10        ; (verify error)
    sta STATUS
    bne lfba7       ; (jmp)

.ifdef SJSAVE

jsave:
    ldx FA          ; pa (device#)
    cpx #4
    bcs @n
    jmp FSAVE2
@n:

.ifdef SAVEPARAMS
    jsr frmword2    ; get word value
    bcs mysa_0

    sty SAL
    sta SAL + 1

    jsr frmword2    ; get word value
    bcs mysa_0

    sty EAL
    sta EAL + 1

    ldy SAL
    lda SAL + 1
    sty STAL
    sta STAL + 1
.endif

mysa_0:
    lda #$f1        ; channel
    jsr disk_listen
    jsr iecnamout
    bcs mysa_err

    lda #$61
    jsr disk_listen

    jsr RD300       ; $c1/$c2 --> $ac/$ad

    lda SAL
    jsr jiecout
    lda SAL + 1
    jsr jiecout

.ifdef PRINTADDRESS
    lda #SAL
    jsr print_atadr_2
.else
.ifdef PRINTMESSAGE
    jsr SAVING      ; print "saving" 
.endif
.endif

    ldy #0
mysa_00:
    jsr VPRTY       ; end address?
    bcs mysa_e0     ; yes -->
    lda (SAL),y
    jsr jiecout

    jsr STOP
    bne mysa_02

    jsr junlisten
    jsr disk_close_sa
    jmp LDA0SEC

mysa_02:
    jsr WRT62       ; incr addr
    bne mysa_00

mysa_e0:
    jsr junlisten
    jsr disk_close_sa

.ifdef PRINTADDRESS
    lda #SAL
    jsr print_toadr_2
.endif

    clc
mysa_err:
    rts

.endif

;;; Write name to IEC and unlisten.
iecnamout:
    lda STATUS
    bmi dicm_err1

    jsr iecnamout_2

dicm_ok2:
    jsr junlisten

dicm_ok:
    clc
    rts

;;; Write name to IEC. TODO: Inline.
iecnamout_2:
    ldx LFN
    beq dicm_ok2
    ldy #0
dicm_2:
    lda (FNADR),y
    jsr jiecout
    iny
    dex
    bne dicm_2
    rts

dicm_err1:
    jmp FE_DVNTP    ; Error DEVICE NOT PRESENT.

disk_listen:
    pha
    lda #0
    sta STATUS
    beq dili_2

disk_listen_2:
    pha

dili_2:
    lda FA
    jsr jlisten
    pla
    jsr jlistensa
dita_5:
    lda STATUS
    bpl dicm_ok
    sec
    rts

disk_talk:
    pha
    lda #0
    sta STATUS

    lda FA
    jsr jtalk
    pla
    jmp jtalksa

disk_close_sa:
    lda #$e1
    bne dicl_1

disk_close_lo:
    jsr juntalk
    lda #$e0

dicl_1:
    jsr disk_listen_2
    jmp junlisten

.ifdef LOADPARAMS
LSPARAMS = 1
.endif
.ifdef SAVEPARAMS
.ifndef LSPARAMS
LSPARAMS = 1
.endif
.endif

.ifdef LSPARAMS
;;; Get word value in y/a and (ptr)
frmword2:
    jsr chkcom
    bcs frwo_3
frmword:
    jsr TYPCHK
    jsr MAKADR
    clc
frwo_3:
    rts
.endif

; TODO: Inline.
chkcom:
    jsr CHRGOT
    cmp #','
    sec
    bne chco_3
    jsr CHRGET
    clc
chco_3:
    rts

.ifdef PRINTADDRESS

;;; Print load at address.
print_atadr:
    lda #EAL

print_atadr_2:
    ldx MSGFLG
    bmi l1      ; TODO: bpl to rts.
    rts

l1: pha
    lda #<txt_from
    ldy #>txt_from
    jsr PRTSTR
    pla

hexoutl3:
    tax
hexout_zp:
    lda 1,x
    pha
    lda 0,x
    tax
    pla
    jmp hexout

;;; Print load address:
print_toadr:
    lda #EAL

print_toadr_2:
    ldx MSGFLG
    bpl lrts2

    pha
    lda #<txt_to
    ldy #>txt_to
    jsr PRTSTR
    pla
    jsr hexoutl3

crout:
    lda #13
    jmp BSOUT

lrts2:
    rts

;;; Print AX in hexadecimal notation.
hexout:
    pha
    lda #'$'
    jsr BSOUT
    pla
    beq hex0
    jsr hex2
hex0:
    txa
hex2:
    pha
    lsr
    lsr
    lsr
    lsr
    jsr hex1
    pla
    and #15
hex1:
    clc
    adc #246
    bcc hex1_2
    adc #6
hex1_2:
    adc #58
    jmp BSOUT

    .rodata

txt_from:   .byte " FROM ",0
txt_to:     .byte " TO ",0

.endif

.ifdef FULLKERNAL

    .code

jchkin:
    jsr FNDFLNO     ; Search logical file number.
    beq @l1
    jmp FE_NTOPN    ; Error FILE NOT OPEN.

@l1:jsr SETFLCH     ; Set file parametres.
    lda FA
    cmp #8
    bcs @l2
    jmp FCHKIN_2

@l2:tax
    jsr jtalk
    lda SA
    bpl @l3
    jmp FCHKIN_3

@l3:jsr jtalksa
    jmp FCHKIN_4

jchkout:
    jsr FNDFLNO     ; Search logical file number.
    beq @l1
    jmp FE_NTOPN    ; Error FILE NOT OPEN.

@l1:jsr SETFLCH     ; Set file parametres.
    lda FA
    cmp #8
    bcs @l2
    jmp FCHKOUT_2

@l2:tax
    jsr jlisten
    lda SA
    bpl @l3
    jmp FCHKOUT_3

@l3:jsr jlistensa
    jmp FCHKOUT_4

jgetin:
    lda DFLTN
    cmp #8
    bcs @l2
    jmp FGETIN

    ; TODO: double with jbasin ¿ forgot the workaround.
@l2:lda STATUS
    beq @l3
    jmp CHRINSR_2
@l3:jmp jiecin

jbasin:
    lda DFLTN
    cmp #8
    bcs @l2
    jmp FCHRIN

@l2:lda STATUS
    beq @l3
    jmp CHRINSR_2

@l3:jmp jiecin

jbasout:
    pha
    lda DFLTO
    cmp #8
    bcs @l2
    jmp FCHROUT_2

@l2:pla
    jmp jiecout

jclrall:
    lda #0
    sta LDTND

jclrch:
    ldx #3
    cpx DFLTO
    bcs @l1
    jsr junlisten
@l1:cpx DFLTN
    bcs @l2
    jsr juntalk
@l2:jmp FCLRCHN_2

.endif
