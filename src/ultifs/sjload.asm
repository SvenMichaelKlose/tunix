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

SJLOAD_START  = $9400

STANDALONE      = 0 ; Make LOADable stand-alone binary.
SJLOAD          = 1 ; Extend LOAD.
LOADPARAMS      = 1 ; Support additional load parameters.
SJSAVE          = 1 ; Extend SAVE.
SAVEPARAMS      = 1 ; Support additional SAVE parameters.
FULLKERNAL      = 1 ; Extend all other KERNAL I/O functions.
PRINTADDRESS    = 1 ; LOAD/SAVE: Print start/end address
PRINTMESSAGE    = 0 ; When PRINTADDRESS == 0: Print saving/loading message like standard kernal routines.

    .code

.ifdef STANDALONE

    .org SJLOAD_START - 2
    .byte <SJLOAD_START, >SJLOAD_START

.endif

ptr         = $14

CHRGET      = $73     ;get next char
CHRGOT      = $79     ;get last char

STATUS      = $90
VERCK       = $93
MSGFLG      = $9d    ; Use to check if RUNning a program.
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

PRTSTR      = $cb1e ;string in ac/yr ausgeben
BSOUT       = $ffd2
TYPCHK      = $cd8a ; get numeric value
MAKADR     = $d7f7 ; convert to word value into y/a; $14 (ptr)
STOP     = $ffe1 ; check stop key

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

; ==============================================================
; jiffy procs
; ==============================================================

;--------------jiffy listen
jtalk:
    ora #$40
    .byte $2c

jlisten:
    ora #$20
    jsr $f160       ;set timer

lee1c:
    pha
    bit $94
    bpl l6e2b
    sec
    ror $a3
    jsr new_iecout  ;new byte out
    lsr $94
    lsr $a3
l6e2b:
    pla
    sta $95

    sei
    lda #$00
    sta $a3
    jsr $e4a0       ;dav hi

    cmp #$3f
    bne l6e38
    jsr $ef84       ;ndac lo
l6e38:
    lda $911f
    ora #$80
    sta $911f

lee40:
    jsr $ef8d       ;pcr bit 1 lÖschen
    jsr $e4a0
    jsr $ef96

old_iecout:
    sei
    jsr $e4a0       ;dav lo
    jsr $e4b2       ;nrfd hi
    lsr
    bcs l6eb4       ;err dev not pres

    jsr $ef84       ;ndac lo
    bit $a3
    bpl l6e66
l6e5a:
    jsr $e4b2       ;nrfd hi
    lsr
    bcc l6e5a
l6e60:
    jsr $e4b2       ;nrfd hi
    lsr
    bcs l6e60
l6e66:
    jsr $e4b2       ;nrfd hi
    lsr
    bcc l6e66
    jsr $ef8d       ;pcr bit 1 lÖschen

    txa
    pha
    ldx #$08        ;8 bit

l6e73:
    lda $911f
    and #$02
    bne l6e7f
    pla
    tax
    jmp $eeb7       ;err timeout

l6e7f:
    jsr $e4a0       ;dav hi
    ror $95
    bcs l6e89
    jsr $e4a9       ;dav lo
l6e89:
    jsr $ef84       ;ndac lo
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
    jmp $eea0

l6eb4:
    jmp $eeb4       ;err dev not pres

l6eb7:
    jmp $eeb7       ;err time out

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
    beq lf995
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

;--------------jiffy byte in
jiecin:
lfbe0:              ;new byte in??
    sei
    bit $a3
    bvs jiffy_in
    lda #$00
    jmp $ef1c       ;orig byte in

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
    jsr lec4e       ;byte aus 2 nibbles

    sta $a4
    pla
    tax
    lda $9c
    ror
    ror
    bpl l7c54
    bcc lfc4f
    lda #$42
    jmp $eeb9       ;err status, unlisten

;--------------jiffy byte in

;--------------jiffy byte out
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
    jsr $fe6a       ;set status

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
    lda lfcce,x
    pha
    txa
    lsr
    lsr
    tax
    lda lfcce,x
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
    lda lfbba,x
    ora $9c
    nop
    sta $912c
    lda lf39e,x
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
    jmp $eeb7       ; err time out

;--------------jiffy byte out

;--------------baut ein byte aus 2 nibbles zusammen
lec4e:
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

;--------------jiffy byte in

;--------------jiffy untalk/unlisten
juntalk:
leef6:
    lda $911f
    ora #$80        ;atn ausgeben
    sta $911f
    jsr $ef8d
    lda #$5f
    .byte $2c


junlisten:
    lda #$3f
    jsr lee1c       ;part of listen
    jsr $eec5
    txa
    ldx #$0b
lef0f:
    dex
    bne lef0f
    tax
    jsr $ef84
    jmp $e4a0

;--------------jiffy untalk/unlisten

;--------------jiffy talk sa
jtalksa:
    sta $95
    jsr lee40
    jmp $eed3

;--------------jiffy talk sa

;--------------jiffy listen sa
jlistensa:
    sta $95
    jsr lee40
    jmp $eec5

;--------------jiffy listen sa

    .rodata

;--------------jiffy data table
lfcce:  .byte $00,$02,$20,$22,$00,$02,$20,$22,$00,$02,$20,$22,$00,$02,$20,$22
lfbba:  .byte $00,$00,$20,$20,$00,$00,$20,$20,$02,$02,$22,$22,$02,$02,$22,$22
lf39e:  .byte $00,$20,$00,$20,$02,$22,$02,$22,$00,$20,$00,$20,$02,$22,$02,$22

; ==============================================================
; sys procs
; ==============================================================

    .code

; :: "fnam",pa,sa[,loadadr]
jload:
    ldx FA       ; pa (device#)
    cpx #4
    bcs @dont_verify
    jmp $f549       ; old load proc

@dont_verify:
    sta VERCK
    lda #0
    sta LA       ; file# - flag for first byte

.ifndef PRINTADDRESS 
.ifdef PRINTMESSAGE
    jsr $f647 ;print "searching" 
.endif
.endif

    lda #$f0        ; channel
    jsr disk_listen
    jsr iecnamout
    bcc l00a
    rts

l00a:
    ldy #$00
    lda ($bb),y     ;filename
    cmp #'$'
    bne l00b        ;directory? -->
    jmp $f56d       ;normal load

l00b:
    lda #$60
    jsr disk_talk

    jsr jiecin      ; load address lo
    sta EAL

    lda STATUS
    lsr
    lsr
    bcc l00c
    jmp $f787

l00c:
    jsr jiecin      ; load address hi
    sta EAL + 1

    ldx SA       ; sa
.ifdef LOADPARAMS
    beq l00         ; sa=0 -->
    dex
    dex
.endif
    bne l01         ; sa=1 -->

.ifdef LOADPARAMS
l02:                ; sa=2: load cartridge at $c3
    pha
    lda EAL
    pha             ; first two bytes ...
.endif

l00:                ; sa=0: load program at $c3
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
    jsr $f66a       ;print "loading / verifying"  
.endif
.endif

    ldx SA      ; sa

.ifdef LOADPARAMS

    dex
    dex
    bne l3         ; sa!=2 -->

    ;store first two bytes
    ldy #0
    pla
    jsr storebyte
    pla
    jsr storebyte

l3:
.endif

;--------------jiffy fastload init
fastload:
    bit $a3
    bvs lfb1f      ;jiffy -->
lf253:
    jsr $f58a      ;normales load
lerr2:
    bcc n
    jmp $f6cb      ;unlisten, close, break
n:

mylo_e:
.ifdef PRINTADDRESS
    jsr print_toadr
.endif

    jsr disk_close_lo

    clc
    ldx EAL
    ldy EAL + 1
    rts

;--------------jiffy fastload init
lfb1f:
    jsr juntalk     ;untalk
    lda #$61
    jsr disk_talk
;--------------jiffy fastload start
    sei
    lda $b2
    pha
    ldy #$00

lfb25:
    jsr $f755      ;stop taste abfragen
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
    .byte $2c      ;bit abs
lfb54:
    lda #$40
    jsr $fe6a      ;set status
    clc
    .byte $24      ;bit zp
lfb5b:             ;stop!
    sec
    pla
    sta $b2
    bcc mylo_e
lerr:
    jmp $f6cb      ;unlisten, close, break

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

    lda #>(lfb6e -1)    ;rücksprungadresse auf stack
    pha
    lda #<(lfb6e -1)
    pha
    jsr lec4e           ;byte zusammenbauen aus 2 nibble

storebyte:
    cpy $93
    bne lfbb0
    sta ($ae),y
lfba7:
    inc $ae
    bne @n
    inc $af
@n: rts

lfbb0:              ;verify
    cmp ($ae),y
    beq lfba7
    lda #$10        ;verify error
    sta $90
    bne lfba7       ; (jmp)

;--------------jiffy fastload end

.ifdef SJSAVE

; save vector     :: "fnam",pa,sa[,fromadr,toaddr]
jsave:
    ldx FA       ; pa (device#)
    cpx #4
    bcs @n
    jmp $f685       ; old load proc
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

    jsr $fbd2       ; $c1/$c2 --> $ac/$ad

    lda SAL
    jsr jiecout
    lda SAL + 1
    jsr jiecout

.ifdef PRINTADDRESS
    lda #SAL
    jsr print_atadr_2
.else
.ifdef PRINTMESSAGE
    jsr $f728       ; print "saving" 
.endif
.endif

    ldy #0
mysa_00:
    jsr $fd11       ;end address?
    bcs mysa_e0     ;yes -->
    lda (SAL),y
    jsr jiecout

    jsr STOP
    bne mysa_02

    jsr junlisten
    jsr disk_close_sa
    jmp $f6ce

mysa_02:
    jsr $fd1b       ;incr addr
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

;put name to iec and unlisten
iecnamout:
    lda STATUS
    bmi dicm_err1

    jsr iecnamout_2

dicm_ok2:
    jsr junlisten

dicm_ok:
    clc
    rts

;put name to iec
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
    jmp $f78a       ;err 'device not present'    cf=1

disk_listen:
    pha
    lda #0
    sta STATUS
    beq dili_2

disk_listen_2:
    pha

dili_2:
    lda FA       ; device#
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

    lda FA       ; device#
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
; get word value in y/a and (ptr)
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

  ; print load at address
print_atadr:
    lda #EAL

print_atadr_2:
    ldx MSGFLG
    bmi l1
lrts:
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

; print load address
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

txt_from:   .byte " from ",0
txt_to:     .byte " to ",0

.endif

.ifdef FULLKERNAL

; ==============================================================
; jiffy io code (chkin, chkout, basin, BSOUT)   from sjload-128
; ==============================================================

    .code

;
; jiffy chkin    ($031e vector)
;
jchkin:
    jsr $f3cf       ;search logical file#
    beq @l1         ;file not open error
    jmp $f784       ;err "file not open"

@l1:jsr $f3df       ;set file param
    lda FA       ;device#
    cmp #8
    bcs @l2
    jmp $f2d2       ;std. chkin

@l2:tax
    jsr jtalk
    lda SA
    bpl @l3
    jmp $f2f8

@l3:jsr jtalksa
    jmp $f301

;
; jiffy chkout    ($0320 vector)
;
jchkout:
    jsr $f3cf       ;search logical file#
    beq @l1         ;file not open error
    jmp $f784       ;err "file not open"

@l1:jsr $f3df       ;set file param
    lda FA       ;device#
    cmp #8
    bcs @l2
    jmp $f314       ;std. chkout

@l2:tax
    jsr jlisten
    lda SA
    bpl @l3
    jmp $f33a

@l3:jsr jlistensa
    jmp $f342

;
; jiffy GETIN    ($032a vector)
;
jgetin:
    lda $99         ;device#
    cmp #8
    bcs @l2
    jmp $f1f5       ;std. getin

    ; TODO: double with jbasin ¿ forgot the workaround.
@l2:lda STATUS
    beq @l3
    jmp $f268       ;std. iecin
@l3:jmp jiecin

;
; jiffy basin    ($0324 vector)
;
jbasin:
    lda $99         ;device#
    cmp #8
    bcs @l2
    jmp $f20e       ;std. basin

@l2:lda STATUS
    beq @l3
    jmp $f268       ;std. iecin

@l3:jmp jiecin

;
; jiffy basout    ($0326 vector)
;
jbasout:
    pha
    lda $9a         ;device#
    cmp #8
    bcs @l2
    jmp $f27b         ;std. basout

@l2:pla
    jmp jiecout

;
; jiffy clrch / clrall    ($0322 / $032c vector)
;
jclrall:
    lda #0
    sta $98

jclrch:
    ldx #3
    cpx $9a         ;device# out
    bcs @l1
    jsr junlisten
@l1:cpx $99         ;device# in
    bcs @l2
    jsr juntalk
@l2:jmp $f403       ;std. clrall
.endif
