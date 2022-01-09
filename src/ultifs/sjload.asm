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
;   ca65 version and reformatting by Sven Michael Klose <pixel@hugbox.org>

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

my_wedge_lo = $0500

f_io     = 1000     ;io flag
f_we     = 1001     ;wedge flag
f_curdev = 1002     ;wedge flag

flgcom  = $08

chrptr  = $7a       ;char pointer
pt1     = $22       ;pointer
pt2     = $24       ;pointer
pt3     = $14       ;pointer

fac     = $61

c_line  = $d1       ;pointer current line char ram
c_colp  = $f3       ;pointer current line color ram

c_ctrl  = $d4       ;control mode

c_row   = $d6       ;cursor row
c_col   = $d3       ;cursor column
c_chr   = $d7       ;cuurent char


basstrt = $2b       ;basic start
basvar  = $2d       ;basic vars
basarr  = $2f       ;basic arrays
basaend = $31       ;basic arrays end
basstr  = $33       ;basic strings
strptr  = $35       ;string pointer
basend  = $37       ;basic end

savestart = $c1
loadptr   = $c3
loadstart = $ac
loadend   = $ae

keyanz  = $c6

iecstat  = $90

len_fnam = $b7
ptr_fnam = $bb

sy_verify   = $93
sy_status   = $90
sy_sa       = $b9
sy_dn       = $ba
sy_fn       = $b8

direct_mode = $9d   ;direct=$80/run=0

chrget  = $0073     ;get next char
chrgot  = $0079     ;get last char

bip     = $0200     ;basic input buffer 88 bytes
cas_buf = 828       ;kassetten buffer

sy_strout  = $cb1e  ;string in ac/yr ausgeben
bsout      = $ffd2
getin      = $ffe4

ptr_error_out   = $0300
ptr_input_loop  = $0302
ptr_frmelem     = $030a
ptr_load        = $0330
ptr_save        = $0332

ptr_chkin       = $031e
ptr_chkout      = $0320
ptr_basin       = $0324
ptr_clrch       = $0322
ptr_basout      = $0326
ptr_getin       = $032a
ptr_clrall      = $032c

;-------------------- wedge init
sjload_init:
    lda #<my_load
    ldx #>my_load
    sta ptr_load
    stx ptr_load + 1

.ifdef SJSAVE
    lda #<my_save
    ldx #>my_save
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
jif_talk:
    ora #$40
    .byte $2c

jif_listen:
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
jif_iecin:
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
jif_iecout:
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
jif_untalk:
leef6:
    lda $911f
    ora #$80        ;atn ausgeben
    sta $911f
    jsr $ef8d
    lda #$5f
    .byte $2c


jif_unlisten:
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
jif_talksa:
    sta $95
    jsr lee40
    jmp $eed3

;--------------jiffy talk sa

;--------------jiffy listen sa
jif_listensa:
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

frmnum   = $cd8a    ; get numeric value
frmbyte  = $d79e    ; get byte value to x
cnvword  = $d7f7    ; convert to word value into y/a; $14 (pt3)
setstat  = $fe6a    ; set status
chkstop  = $ffe1    ; check stop key

unlisten = jif_unlisten     ; send unlisten command
untalk   = jif_untalk       ; send untalk command
listen   = jif_listen       ; send listen command
talk     = jif_talk         ; send talk command
iecin    = jif_iecin        ; get char from iec
iecout   = jif_iecout       ; send char to iec

listensa = jif_listensa     ; send sa for listen command
talksa   = jif_talksa       ; send sa for talk command

    .code

; :: "fnam",pa,sa[,loadadr]
my_load:
    ldx sy_dn       ; pa (device#)
    cpx #4
    bcs l0
    jmp $f549       ; old load proc

l0: sta sy_verify
    lda #0
    sta sy_fn       ; file# - flag for first byte

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

    jsr iecin       ; load address lo
    sta loadend

    lda sy_status
    lsr
    lsr
    bcc l00c
    jmp $f787

l00c:
    jsr iecin       ; load address hi
    sta loadend + 1

    ldx sy_sa       ; sa
.ifdef LOADPARAMS
    beq l00         ; sa=0 -->
    dex
    dex
.endif
    bne l01         ; sa=1 -->

.ifdef LOADPARAMS
l02:                ; sa=2: load cartridge at $c3
    pha
    lda loadend
    pha             ; first two bytes ...
.endif

l00:                ; sa=0: load program at $c3
.ifdef LOADPARAMS
    jsr frmword2    ; get word value
.endif

    lda loadptr + 1
    ldx loadptr
.ifdef LOADPARAMS
    bcs l2

    lda pt3 + 1
    ldx pt3

l2:
.endif

    sta loadend + 1
    stx loadend

l01:
.ifdef PRINTADDRESS
    jsr print_atadr
.else
.ifdef PRINTMESSAGE
    jsr $f66a       ;print "loading / verifying"  
.endif
.endif

    ldx sy_sa      ; sa

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
    ldx loadend
    ldy loadend + 1
    rts

;--------------jiffy fastload init
lfb1f:
    jsr untalk     ;untalk
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
@n:
    rts

lfbb0:              ;verify
    cmp ($ae),y
    beq lfba7
    lda #$10        ;verify error
    sta $90
    bne lfba7       ; (jmp)

;--------------jiffy fastload end

.ifdef SJSAVE

; ========================================================================
; my save                   endaddr   = ($ae/$af)    startaddr = ($c1/$c2)
;
; savestart = $c1
; loadptr   = $c3
; loadstart = $ac
; loadend   = $ae
; ========================================================================

  ; save vector     :: "fnam",pa,sa[,fromadr,toaddr]
my_save:
    ldx sy_dn       ; pa (device#)
    cpx #4
    bcs my_iecsave
    jmp $f685       ; old load proc

my_iecsave:

.ifdef SAVEPARAMS
    jsr frmword2    ; get word value
    bcs mysa_0

    sty loadstart
    sta loadstart + 1

    jsr frmword2    ; get word value
    bcs mysa_0

    sty loadend
    sta loadend + 1

    ldy loadstart
    lda loadstart + 1
    sty savestart
    sta savestart + 1
.endif

mysa_0:
    lda #$f1        ; channel
    jsr disk_listen
    jsr iecnamout
    bcs mysa_err

    lda #$61
    jsr disk_listen

    jsr $fbd2       ; $c1/$c2 --> $ac/$ad

    lda loadstart
    jsr iecout
    lda loadstart + 1
    jsr iecout

.ifdef PRINTADDRESS
    lda #loadstart
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
    lda (loadstart),y
    jsr iecout

    jsr chkstop
    bne mysa_02

    jsr unlisten
    jsr disk_close_sa
    jmp $f6ce

mysa_02:
    jsr $fd1b       ;incr addr
    bne mysa_00

mysa_e0:
    jsr unlisten
    jsr disk_close_sa

.ifdef PRINTADDRESS
    lda #loadstart
    jsr print_toadr_2
.endif

    clc
mysa_err:
    rts

.endif

;put name to iec and unlisten
iecnamout:
    lda iecstat
    bmi dicm_err1

    jsr iecnamout_2


dicm_ok2:
    jsr unlisten


dicm_ok:
    clc
    rts

;put name to iec
iecnamout_2:
    ldx len_fnam
    beq dicm_ok2
    ldy #0
dicm_2:
    lda (ptr_fnam),y
    jsr iecout
    iny
    dex
    bne dicm_2
    rts

dicm_err1:
    jmp $f78a       ;err 'device not present'    cf=1

disk_listen:
    pha
    lda #0
    sta iecstat
    beq dili_2

disk_listen_2:
    pha

dili_2:
    lda sy_dn       ; device#
    jsr listen
    pla
    jsr listensa
dita_5:
    lda iecstat
    bpl dicm_ok
    sec
    rts

disk_talk:
    pha
    lda #0
    sta iecstat

    lda sy_dn       ; device#
    jsr talk
    pla
    jmp talksa

disk_close_sa:
    lda #$e1
    bne dicl_1

disk_close_lo:
    jsr untalk
    lda #$e0

dicl_1:
    jsr disk_listen_2
    jmp unlisten

.ifdef LOADPARAMS ; | SAVEPARAMS == 1
; get word value in y/a and (pt3)
frmword2:
    jsr chkcom
    bcs frwo_3
frmword:
    jsr frmnum
    jsr cnvword
    clc
frwo_3:
    rts

.endif

chkcom:
    jsr chrgot
    cmp #','
    sec
    bne chco_3
    jsr chrget
    clc
chco_3:
    rts

.ifdef PRINTADDRESS

  ; print load at address
print_atadr:
    lda #loadend

print_atadr_2:
    ldx direct_mode
    bmi l1
lrts:
    rts

l1:
    pha
    lda #<msg_loadat
    ldy #>msg_loadat
    jsr sy_strout
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
    lda #loadend

print_toadr_2:
    ldx direct_mode
    bpl lrts2

    pha
    lda #<msg_loadto
    ldy #>msg_loadto
    jsr sy_strout
    pla
    jsr hexoutl3

crout:
    lda #13
    jmp bsout

lrts2:
    rts

;------------ print hex value in  x/a
hexout:
    pha
    lda #'$'
    jsr bsout
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
    jmp bsout

; ==============================================================
; message texte
; ==============================================================

    .rodata

msg_loadat: .byte " from ",0
msg_loadto: .byte " to ",0

.endif

.ifdef FULLKERNAL

; ==============================================================
; jiffy io code (chkin, chkout, basin, bsout)   from sjload-128
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
    lda sy_dn       ;device#
    cmp #8
    bcs @l2
    jmp $f2d2       ;std. chkin

@l2:tax
    jsr talk
    lda sy_sa
    bpl @l3
    jmp $f2f8

@l3:jsr talksa
    jmp $f301

;
; jiffy chkout    ($0320 vector)
;
jchkout:
    jsr $f3cf       ;search logical file#
    beq @l1         ;file not open error
    jmp $f784       ;err "file not open"

@l1:jsr $f3df       ;set file param
    lda sy_dn       ;device#
    cmp #8
    bcs @l2
    jmp $f314       ;std. chkout

@l2:tax
    jsr listen
    lda sy_sa
    bpl @l3
    jmp $f33a

@l3:jsr listensa
    jmp $f342

;
; jiffy getin    ($032a vector)
;
jgetin:
    lda $99         ;device#
    cmp #8
    bcs @l2
    jmp $f1f5       ;std. getin

    ; TODO: double with jbasin ¿ forgot the workaround.
@l2:lda sy_status
    beq @l3
    jmp $f268       ;std. iecin
@l3:jmp jif_iecin

;
; jiffy basin    ($0324 vector)
;
jbasin:
    lda $99         ;device#
    cmp #8
    bcs @l2
    jmp $f20e       ;std. basin

@l2:lda sy_status
    beq @l3
    jmp $f268       ;std. iecin

@l3:jmp jif_iecin

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
    jmp jif_iecout

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
    jsr jif_unlisten
@l1:cpx $99         ;device# in
    bcs @l2
    jsr jif_untalk
@l2:jmp $f403       ;std. clrall
.endif
