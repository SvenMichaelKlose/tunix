; SJLOAD/SJSAVE 07  --== MOD VERSION ==--
; ----------------
;
; C64 floppy speeder and disk utility based on VDOS by Edward Carroll 1986
; modified for Jiffy compatibility by 1570 in 2008
; modified for VC-20 and for Jiffy Save by Diddl in 2009
; modified for VC-20 and for basic IO by Diddl in 2010
; v07 version for Vic-20 by nbla000 in 2010
;
; new features by nbla000:
; - Enabled SAVING/LOADING kernal messages when PRINTADDRESS = 0 (IF PRINTMESSAGE = 1)
; - Bug fix, Vic hangs when a drive not present was used
; - Bug fix, Drive not closed when load is finished
; - Removed unused routines to reduce program size (few bytes) 
; - Added PRINTMESSAGE, LOADPARAMS and SAVEPARAMS compilation parameters

START_ADR  = $9400


SJSAVE = 1                              ; 0 = Don't compile SJsave routines   (LOAD only)
                                        ; 1 = compile SJsave routines

BASIO = 1                               ; 0 = Don't compile basic IO routines (LOAD/SAVE only)
                                        ; 1 = compile basic IO routines

PRINTADDRESS = 1                        ; 0 = Don't print start/end address
                                        ; 1 = print start/end address

LOADPARAMS = 1                          ; 0 = Don't compile with additional LOAD parameters
                                        ; 1 = compile with additional LOAD parameters

SAVEPARAMS = 1                          ; 0 = Don't compile with additional SAVE parameters (if SJSAVE == 1 )
                                        ; 1 = compile with additional SAVE parameters (if SJSAVE == 1 )

; This option works if PRINTADDRESS = 0 ONLY

PRINTMESSAGE = 0                        ; 0 = Don't print SAVING/LOADING Message
                                        ; 1 = print SAVING/LOADING Message like standard kernal routines


    .code

  org START_ADR - 2

  .byte <(START_ADR),>(START_ADR)

MY_WEDGE_LO = $0500

F_IO     = 1000                         ;IO FLAG
F_WE     = 1001                         ;WEDGE FLAG
F_CURDEV = 1002                         ;WEDGE FLAG

FLGCOM  = $08

CHRPTR  = $7a                           ;Char Pointer
PT1     = $22                           ;Pointer
PT2     = $24                           ;Pointer
PT3     = $14                           ;Pointer

FAC     = $61

C_LINE  = $d1                           ;pointer current line char RAM
C_COLP  = $f3                           ;pointer current line color RAM

C_CTRL  = $d4                           ;control mode

C_ROW   = $d6                           ;cursor row
C_COL   = $d3                           ;cursor column
C_CHR   = $d7                           ;cuurent char


BASSTRT = $2B                           ;BASIC START
BASVAR  = $2d                           ;BASIC VARS
BASARR  = $2f                           ;BASIC ARRAYS
BASAEND = $31                           ;BASIC ARRAYS END
BASSTR  = $33                           ;BASIC STRINGS
STRPTR  = $35                           ;STRING POINTER
BASEND  = $37                           ;BASIC END

SAVESTART = $c1
LOADPTR   = $c3
LOADSTART = $ac
LOADEND   = $ae

KEYANZ  = $c6

IECSTAT  = $90

LEN_FNAM = $b7
PTR_FNAM = $bb

SY_VERIFY   = $93
SY_STATUS   = $90
SY_SA       = $b9
SY_DN       = $ba
SY_FN       = $b8

DIRECT_MODE = $9d                       ;Direct=$80/RUN=0

CHRGET  = $0073                         ;GET NEXT CHAR
CHRGOT  = $0079                         ;GET LAST CHAR

BIP     = $0200                         ;BASIC Input Buffer 88 Bytes
CAS_BUF = 828                           ;Kassetten Buffer

SY_STROUT  = $cb1e                      ;String in AC/YR ausgeben
BSOUT      = $ffd2
GETIN      = $ffe4

PTR_ERROR_OUT   = $0300
PTR_INPUT_LOOP  = $0302
PTR_FRMELEM     = $030a
PTR_LOAD        = $0330
PTR_SAVE        = $0332

PTR_CHKIN       = $031e
PTR_CHKOUT      = $0320
PTR_BASIN       = $0324
PTR_CLRCH       = $0322
PTR_BASOUT      = $0326
PTR_GETIN       = $032a
PTR_CLRALL      = $032c

#seg code

;-------------------- WEDGE INIT
.proc MY_WEDGE_INIT
  lda #<MY_LOAD
  ldx #>MY_LOAD
  sta PTR_LOAD
  stx PTR_LOAD +1

#if SJSAVE == 1
  lda #<MY_SAVE
  ldx #>MY_SAVE
  sta PTR_SAVE
  stx PTR_SAVE +1
#endif

#if BASIO == 1
  lda #<JChkIn
  ldx #>JChkIn
  sta PTR_CHKIN
  stx PTR_CHKIN +1
  lda #<JChkOut
  ldx #>JChkOut
  sta PTR_CHKOUT
  stx PTR_CHKOUT +1
  lda #<JBasIn
  ldx #>JBasIn
  sta PTR_BASIN
  stx PTR_BASIN +1
  lda #<JBasOut
  ldx #>JBasOut
  sta PTR_BASOUT
  stx PTR_BASOUT +1
  lda #<JGetIn
  ldx #>JGetIn
  sta PTR_GETIN
  stx PTR_GETIN +1
  lda #<JClrCh
  ldx #>JClrCh
  sta PTR_CLRCH
  stx PTR_CLRCH +1
  lda #<JClrAll
  ldx #>JClrAll
  sta PTR_CLRALL
  stx PTR_CLRALL +1
#endif

  rts
.endproc


; ==============================================================
; JIFFY PROCS
; ==============================================================

;--------------JIFFY LISTEN
.proc JIF_TALK
  ORA #$40
  .byte $2c
.endproc

.proc JIF_LISTEN
  ORA #$20
  JSR $F160                             ;SET TIMER
lEE1C:
  PHA
  BIT $94
  BPL l6E2B
  SEC
  ROR $A3
  JSR lfc41                             ;NEW BYTE OUT
  LSR $94
  LSR $A3
l6E2B:
  PLA
  STA $95

  ;JSR lF19A                            ;NEW DAV hi
  SEI
  LDA #$00
  STA $A3
  JSR $E4A0                             ;DAV hi

  CMP #$3F
  BNE l6E38
  JSR $EF84                             ;NDAC lo
l6E38:
  LDA $911F
  ORA #$80
  STA $911F
lEE40:
  JSR $EF8D                             ;PCR BIT 1 LÖSCHEN
  JSR $E4A0
  JSR $EF96
.endproc

.proc OLD_IECOUT
  SEI
  JSR $E4A0                             ;DAV lo
  JSR $E4B2                             ;NRFD hi
  LSR
  BCS l6EB4                             ;err DEV NOT PRES

  JSR $EF84                             ;NDAC lo
  BIT $A3
  BPL l6E66
l6E5A:
  JSR $E4B2                             ;NRFD hi
  LSR
  BCC l6E5A
l6E60:
  JSR $E4B2                             ;NRFD hi
  LSR
  BCS l6E60
l6E66:
  JSR $E4B2                             ;NRFD hi
  LSR
  BCC l6E66
  JSR $EF8D                             ;PCR BIT 1 LÖSCHEN

  TXA
  PHA
  LDX #$08                              ;8 BIT

l6E73:
  LDA $911F
  AND #$02
  BNE l6E7F
  PLA
  TAX
  JMP $EEB7                             ;ERR TIMEOUT

l6E7F:
  JSR $E4A0                             ;DAV hi
  ROR $95
  BCS l6E89
  JSR $E4A9                             ;DAV lo
l6E89:
  JSR $EF84                             ;NDAC lo
  LDA $912C
  AND #$DD
  ORA #$02
  PHP
  PHA
  JSR lF96E
  PLA
  PLP
  DEX
  BNE l6E73

  PLA
  TAX
  jmp $EEA0

l6EB4:
  jmp $eeb4                             ;err DEV NOT PRES

l6EB7:
  jmp $eeb7                             ;err TIME OUT


lF96E:
  STA $912C
  BIT $911F
  BPL lF997
  CPX #$02
  BNE lF997
  LDA #$02
  LDX #$20
lF97E:
  BIT $911F
  BEQ lF988
  DEX
  BNE lF97E
  BEQ lF995
lF988:
  BIT $911F
  BEQ lF988
  LDA $95
  ROR
  ROR
  ORA #$40
  STA $A3
lF995:
  LDX #$02
lF997:
  rts
.endproc

;--------------JIFFY BYTE IN
.proc JIF_IECIN
lfbe0:                                   ;NEW BYTE IN??
  sei
  bit $a3
  bvs l7be5
  LDA #$00
  JMP $EF1C                             ;ORIG BYTE IN
.endproc

.proc JIFFY_IN
l7be5:
  LDA $911F
  AND #$03
  BEQ l7be5
  LDA #$80
  STA $9C
  TXA
  PHA
  PHA
  PLA
  PHA
  PLA
  LDA $912C
  AND #$DD
  STA $912C
  ORA #$20
  TAX
  BIT $9C
  BIT $9C
  BIT $9C
  LDA $911F
  ROR
  ROR
  NOP
  AND #$80
  ORA $911F
  ROL
  ROL
  STA $B3
  LDA $911F
  ROR
  ROR
  AND #$80
  NOP
  ORA $911F
  ROL
  ROL
  STA $C0
  LDA $911F
  STX $912C
  STA $9C
  JSR lEC4E                             ;BYTE AUS 2 NIBBLES

  STA $A4
  PLA
  TAX
  LDA $9C
  ROR
  ROR
  BPL l7C54
  BCC lfC4f
  LDA #$42
  JMP $EEB9                             ;ERR STATUS, UNLISTEN
.endproc
;--------------JIFFY BYTE IN

;--------------JIFFY BYTE OUT
.proc JIF_IECOUT
  BIT $94
  BMI lEEED
  SEC
  ROR $94
  BNE lEEF2
lEEED:
  PHA
  JSR NEW_IECOUT
  PLA
lEEF2:
  STA $95
  CLC
  RTS
.endproc

.proc NEW_IECOUT
lfc41:                                  ;NEW BYTE OUT
  sei
  bit $a3
  bvs lfc59
  LDA $A3
  CMP #$A0
  BCS lfc59
  JMP OLD_IECOUT
  ;JMP $EE49                             ;ORIG BYTE OUT

lfC4f:
  LDA #$40
  JSR $FE6A                             ;SET STATUS
l7C54:
  LDA $A4
l7C56:
  CLI
  CLC
  RTS
.endproc


.proc JIFFY_OUT
lfc59:                                  ;JIFFY BYTE OUT
  TXA
  PHA
  LDA $95
  LSR
  LSR
  LSR
  LSR
  TAX
  LDA lFCCE,X
  PHA
  TXA
  LSR
  LSR
  TAX
  LDA lFCCE,X
  STA $B3
  LDA $95
  AND #$0F
  TAX
  LDA #$02
l7C76:
  BIT $911F
  BEQ l7C76

  LDA $912C
  AND #$DD
  STA $9C
  PHA
  PLA
  PHA
  PLA
  NOP
  NOP
  NOP
  STA $912C
  PLA
  ORA $9C
  NOP
  STA $912C
  LDA $B3
  ORA $9C
  ORA $9C
  STA $912C
  LDA lFBBA,X
  ORA $9C
  NOP
  STA $912C
  LDA lF39E,X
  ORA $9C
  NOP
  STA $912C
  NOP
  AND #$DD
  BIT $A3
  BMI l7CB7
  ORA #$02
l7CB7:
  STA $912C
  PLA
  TAX
  NOP
  LDA $9C
  ORA #$02
  STA $912C
  LDA $911F
  AND #$02
  BEQ l7C56
  JMP $EEB7                             ; err TIME OUT
;--------------JIFFY BYTE OUT

;--------------BAUT EIN BYTE AUS 2 NIBBLES ZUSAMMEN
lEC4E:
  LDA $B3
  AND #$0F
  STA $B3
  LDA $C0
  ASL
  ASL
  ASL
  ASL
  ORA $B3
  RTS
.endproc
;--------------JIFFY BYTE IN

;--------------JIFFY UNTALK/UNLISTEN
.proc JIF_UNTALK
lEEF6:
  LDA $911F
  ORA #$80                              ;ATN ausgeben
  STA $911F
  JSR $EF8D
  LDA #$5F
  .byte $2c
.endproc

.proc
JIF_UNLISTEN
  LDA #$3F
  JSR lEE1C                             ;PART OF LISTEN
  JSR $EEC5
  TXA
  LDX #$0B
lEF0F:
  DEX
  BNE lEF0F
  TAX
  JSR $EF84
  JMP $E4A0
.endproc
;--------------JIFFY UNTALK/UNLISTEN



;--------------JIFFY TALK SA
.proc JIF_TALKSA
  STA $95
  JSR lEE40
  jmp $eed3
.endproc
;--------------JIFFY TALK SA


;--------------JIFFY LISTEN SA
.proc JIF_LISTENSA
  STA $95
  JSR lEE40
  jmp $eec5
.endproc
;--------------JIFFY LISTEN SA


    .rodata

;--------------JIFFY DATA TABLE
lFCCE:  .byte $00,$02,$20,$22,$00,$02,$20,$22,$00,$02,$20,$22,$00,$02,$20,$22
lFBBA:  .byte $00,$00,$20,$20,$00,$00,$20,$20,$02,$02,$22,$22,$02,$02,$22,$22
lF39E:  .byte $00,$20,$00,$20,$02,$22,$02,$22,$00,$20,$00,$20,$02,$22,$02,$22


; ==============================================================
; SYS PROCS
; ==============================================================

FRMNUM   = $cd8a	                ; GET NUMERIC VALUE
FRMBYTE  = $d79e	                ; GET BYTE VALUE TO X
CNVWORD  = $d7f7	                ; CONVERT TO WORD VALUE INTO Y/A; $14 (PT3)
SETSTAT  = $fe6a	                ; set status
CHKSTOP  = $ffe1                        ; check stop key

UNLISTEN = JIF_UNLISTEN	                ; send UNLISTEN command
UNTALK   = JIF_UNTALK	                ; send UNTALK command
LISTEN	 = JIF_LISTEN                   ; send LISTEN command
TALK     = JIF_TALK	                ; send TALK command
IECIN    = JIF_IECIN	                ; get char from IEC
IECOUT   = JIF_IECOUT	                ; send char to IEC

LISTENSA = JIF_LISTENSA                 ; send SA for LISTEN command
TALKSA   = JIF_TALKSA	                ; send SA for TALK command

    .code

; :: "fnam",PA,SA[,loadadr]
.proc MY_LOAD
  ldx SY_DN                             ; PA (device#)
  cpx #4
  bcs l0
  jmp $f549                             ; OLD LOAD PROC

l0:
  sta SY_VERIFY
  lda #0
  sta SY_FN                             ; file# - flag for first byte

#if PRINTADDRESS == 0 & PRINTMESSAGE == 1
  jsr $f647 ;Print "SEARCHING" 
#endif

  lda #$f0                              ; channel
  jsr DISK_LISTEN
  jsr IECNAMOUT
  bcc l00a
  rts

l00a:
  LDY #$00
  LDA ($BB),Y                           ;Filename
  CMP #$24                              ;"$"
  BNE l00b                              ;Directory? -->
  jmp $f56d                             ;normal LOAD

l00b:
  lda #$60
  jsr DISK_TALK

  jsr IECIN                             ; load address lo
  sta LOADEND

  lda SY_STATUS
  lsr
  lsr
  bcc l00c
  jmp $f787

l00c:
  jsr IECIN                             ; load address hi
  sta LOADEND + 1

  ldx SY_SA                             ; SA
#if LOADPARAMS == 1
  beq l00                               ; SA=0 -->
  dex
  dex
#endif
  bne l01                               ; SA=1 -->

#if LOADPARAMS == 1 
l02:                                    ; SA=2: LOAD CARTRIDGE AT $c3
  pha
  lda LOADEND
  pha                                   ; FIRST TWO BYTES ...
#endif

l00:                                     ; SA=0: LOAD PROGRAM AT $c3
#if LOADPARAMS == 1 
  jsr FRMWORD2                          ; GET WORD VALUE
#endif

  lda LOADPTR +1
  ldx LOADPTR
#if LOADPARAMS == 1 
  bcs l2

  lda PT3 +1
  ldx PT3

l2
#endif

  sta LOADEND +1
  stx LOADEND

l01:
#if PRINTADDRESS == 1
  jsr PRINT_ATADR
#else
#if PRINTMESSAGE == 1
  jsr $f66a ;Print "LOADING / VERIFYING"  
#endif
#endif

  ldx SY_SA                             ; SA

#if LOADPARAMS == 1 

  dex
  dex
  bne l3                                ; SA!=2 -->

  ;STORE FIRST TWO BYTES
  ldy #0
  pla
  jsr STOREBYTE
  pla
  jsr STOREBYTE

l3:
#endif
.endproc

;--------------JIFFY FASTLOAD INIT
.proc .FASTLOAD
  BIT $A3
  BVS lFB1F                             ;Jiffy -->
lF253
  JSR $F58A                             ;normales LOAD
lerr2
  bcs lerr

MYLO_E:
#if PRINTADDRESS == 1
  jsr PRINT_TOADR
#endif

  jsr DISK_CLOSE_LO

  clc
  ldx LOADEND
  ldy LOADEND +1
  rts
.endproc

;--------------JIFFY FASTLOAD INIT
.proc .FB1F
  JSR UNTALK                            ;UNTALK
  lda #$61
  jsr DISK_TALK
;--------------JIFFY FASTLOAD START
  SEI
  LDA $B2
  PHA
  LDY #$00
lFB25:
  JSR $F755                             ;STOP Taste abfragen
  CMP #$FE
  BEQ lFB5B
  LDA $912C
  AND #$DD
  TAX
  ORA #$20
  STA $B2
  STX $912C
  LDA #$80
  STA $9C
lFB3D:
  LDA $911F
  LSR
  BCC .FB3D
  AND #$01
  BEQ .FB67
  LDX #$6D
lFB49:
  BIT $911F
  BEQ .FB54
  DEX
  BNE .FB49
  LDA #$42
  .byte $2c                             ;BIT ABS
lFB54:
  LDA #$40
  JSR $FE6A                             ;SET STATUS
  CLC
  .byte $24                             ;BIT ZP
lFB5B:                                  ;STOP!
  SEC
  PLA
  STA $B2
  bcc .MYLO_E
lerr
  JMP $F6CB                             ;UNLISTEN, CLOSE, BREAK
; JMP $F5BF                             ;UNLISTEN, CLOSE
.endproc

.proc .FB67
  LDA #$02
lFB69
  BIT $911F
  BEQ lFB69
lFB6E
  PHA
  PLA
  NOP
  LDA $B2
  STA $912C
  LDA #$01
  BIT $911F
  BEQ .FB25
  STX $912C
  LDA $911F
  ROR
  ROR
  NOP
  AND #$80
  ORA $911F
  ROL
  ROL
  NOP
  STA $B3
  LDA $911F
  ROR
  ROR
  AND $9C
  ORA $911F
  ROL
  ROL
  STA $C0

  lda #>(.FB6E -1)                      ;Rücksprungadresse auf Stack
  pha
  lda #<(.FB6E -1)
  pha
  JSR lEC4E                             ;Byte zusammenbauen aus 2 Nibble
.endproc

.proc STOREBYTE
  CPY $93
  BNE lFBB0
  STA ($AE),Y
lFBA7:
  INC $AE
  BNE lFB6E
  INC $AF
lFB6E:
  rts

lFBB0:                                  ;VERIFY
  CMP ($AE),Y
  BEQ lFBA7
  LDA #$10                              ;VERIFY ERROR
  STA $90
  BNE lFBA7         ; (jmp)
.endproc
;--------------JIFFY FASTLOAD END

#if SJSAVE == 1

; ========================================================================
; MY SAVE                   ENDADDR   = ($AE/$AF)    STARTADDR = ($C1/$C2)
;
; SAVESTART = $c1
; LOADPTR   = $c3
; LOADSTART = $ac
; LOADEND   = $ae
; ========================================================================

  ; SAVE VECTOR                         :: "fnam",PA,SA[,fromadr,toaddr]
.proc MY_SAVE
  ldx SY_DN                             ; PA (device#)
  cpx #4
  bcs MY_IECSAVE
  jmp $f685                             ; OLD LOAD PROC
.endproc

.proc MY_IECSAVE

#if SAVEPARAMS == 1 
  jsr FRMWORD2                          ; GET WORD VALUE
  bcs MYSA_0

  sty LOADSTART
  sta LOADSTART +1

  jsr FRMWORD2                          ; GET WORD VALUE
  bcs MYSA_0

  sty LOADEND
  sta LOADEND +1

  ldy LOADSTART
  lda LOADSTART +1
  sty SAVESTART
  sta SAVESTART +1
#endif

MYSA_0
  lda #$f1                              ; channel
  jsr DISK_LISTEN
  jsr IECNAMOUT
  bcs MYSA_ERR

  lda #$61
  jsr DISK_LISTEN

  jsr $fbd2                             ; $C1/$C2 --> $ac/$ad

  lda LOADSTART
  jsr IECOUT
  lda LOADSTART +1
  jsr IECOUT

#if PRINTADDRESS == 1
  lda #LOADSTART
  jsr PRINT_ATADR_2
#else
#if PRINTMESSAGE == 1  
  jsr $f728 ; Print "SAVING" 
#endif
#endif

  ldy #0
MYSA_00:
  jsr $fd11                             ;END ADDRESS?
  bcs MYSA_E0                           ;YES -->
  lda (LOADSTART),y
  jsr IECOUT

  jsr CHKSTOP
  bne MYSA_02

  jsr UNLISTEN
  jsr DISK_CLOSE_SA
  jmp $f6ce

MYSA_02:
  jsr $fd1b                             ;INCR ADDR
  bne MYSA_00

MYSA_E0:
  jsr UNLISTEN
  jsr DISK_CLOSE_SA

#if PRINTADDRESS == 1
  lda #LOADSTART
  jsr PRINT_TOADR_2
#endif

;  jsr PRINT_DISK_ERR
  clc
MYSA_ERR:
  rts

#endif
.endproc

;PUT NAME TO IEC AND UNLISTEN
.proc IECNAMOUT
  lda IECSTAT
  bmi DICM_ERR1

  jsr IECNAMOUT_2
DICM_OK2:
  jsr UNLISTEN
DICM_OK:
  clc
  rts

;PUT NAME TO IEC
IECNAMOUT_2:
  ldx LEN_FNAM
  beq DICM_OK2
  ldy #0
DICM_2:
  lda (PTR_FNAM),y
  jsr IECOUT
  iny
  dex
  bne DICM_2
  rts

DICM_ERR1:
  jmp $f78a                             ;ERR 'DEVICE NOT PRESENT'    CF=1
.endproc

.proc DISK_LISTEN
  pha
  lda #0
  sta IECSTAT
  beq DILI_2

DISK_LISTEN_2:
  pha
DILI_2:
  lda SY_DN                             ; device#
  jsr LISTEN
  pla
  jsr LISTENSA
DITA_5:
  lda IECSTAT
  bpl DICM_OK
  sec
  rts
.endproc

.proc DISK_TALK
  pha
  lda #0
  sta IECSTAT

  lda SY_DN                             ; device#
  jsr TALK
  pla
  jmp TALKSA
.endproc

.proc DISK_CLOSE_SA
  lda #$e1
  bne DICL_1

DISK_CLOSE_LO:
  jsr UNTALK
  lda #$e0
DICL_1:
  jsr DISK_LISTEN_2
  jmp UNLISTEN
.endproc

#if LOADPARAMS == 1 | SAVEPARAMS == 1
; GET WORD VALUE IN Y/A AND (PT3)
.proc FRMWORD2
  jsr CHKCOM
  bcs FRWO_3
FRMWORD:
  jsr FRMNUM
  jsr CNVWORD
  clc
FRWO_3:
  rts
.endproc
#endif

.proc CHKCOM
  jsr CHRGOT
  cmp #$2c                                ; ","
  sec
  bne CHCO_3
  jsr CHRGET
  clc
CHCO_3:
  rts
.endproc

#if PRINTADDRESS == 1

  ; PRINT LOAD AT ADDRESS
.proc PRINT_ATADR subroutine
  lda #LOADEND
PRINT_ATADR_2:
  ldx DIRECT_MODE
  bmi l1
lrts:
  rts

l1:
  pha
  lda #<MSG_LOADAT
  ldy #>MSG_LOADAT
  jsr SY_STROUT
  pla
l3:
  tax
HEXOUT_ZP:
  lda 1,x
  pha
  lda 0,x
  tax
  pla
  jmp HEXOUT

; PRINT LOAD ADDRESS
PRINT_TOADR:
  lda #LOADEND
PRINT_TOADR_2
  ldx DIRECT_MODE
  bpl lrts

  pha
  lda #<MSG_LOADTO
  ldy #>MSG_LOADTO
  jsr SY_STROUT
  pla
  jsr l3

CROUT:
  lda #13
  jmp BSOUT
.endproc

;------------ PRINT HEX VALUE IN  X/A
.proc HEXOUT
  pha
  lda #"$"
  jsr BSOUT
  pla
  beq HEX0
  jsr HEX2
HEX0:
  txa
HEX2:
  pha
  lsr
  lsr
  lsr
  lsr
  jsr HEX1
  pla
  and #15
HEX1:
  clc
  adc #246
  bcc HEX1_2
  adc #6
HEX1_2:
  adc #58
  jmp BSOUT
.endproc

; ==============================================================
; MESSAGE TEXTE
; ==============================================================

MSG_LOADAT: .byte " FROM ",0
MSG_LOADTO: .byte " TO ",0

#endif

#if BASIO == 1

; ==============================================================
; JIFFY IO CODE (CHKIN, CHKOUT, BASIN, BSOUT)   from SJLOAD-128
; ==============================================================

;
; JIFFY CHKIN    ($031e vector)
;
.proc JChkIn
  jsr $f3cf                             ;search logical file#
  beq l1	                        ;file not open error
  jmp $f784                             ;err "file not open"

l1:
  jsr $f3df                             ;set file param
  lda SY_DN                             ;device#
  cmp #8
  bcs l2
  jmp $f2d2                             ;std. ChkIn

l2:
  tax
  jsr TALK
  lda SY_SA
  bpl l3
  jmp $f2f8

l3:
  jsr TALKSA
  jmp $f301
.endproc

;
; JIFFY CHKOUT    ($0320 vector)
;
.proc JChkOut
  jsr $f3cf                             ;search logical file#
  beq l1	                        ;file not open error
  jmp $f784                             ;err "file not open"

l1:
  jsr $f3df                             ;set file param
  lda SY_DN                             ;device#
  cmp #8
  bcs l2
  jmp $f314                             ;std. ChkOut

l2:
  tax
  jsr LISTEN
  lda SY_SA
  bpl l3
  jmp $f33a

l3:
  jsr LISTENSA
  jmp $f342
.endproc

;
; JIFFY GETIN    ($032a vector)
;
.proc JGetIn
  lda $99                               ;device#
  cmp #8
  bcs .2
  jmp $f1f5                             ;std. GetIn
.endproc

;
; JIFFY BASIN    ($0324 vector)
;
.proc JBasIn
  lda $99                               ;device#
  cmp #8
  bcs l2
  jmp $f20e                             ;std. BasIn

l2:
  lda SY_STATUS
  beq l3
  jmp $f268                             ;std. IECIN

l3:
  jmp JIF_IECIN
.endproc

;
; JIFFY BASOUT    ($0326 vector)
;
.proc JBasOut
  pha
  lda $9a                               ;device#
  cmp #8
  bcs l2
  jmp $f27b                             ;std. BasOut

l2:
  pla
  jmp JIF_IECOUT
.endproc

;
; JIFFY CLRCH / CLRALL    ($0322 / $032c vector)
;
.proc JClrAll
  lda #0
  sta $98

JClrCh:
  ldx #3
  cpx $9a                               ;device# out
  bcs l1
  jsr JIF_UNLISTEN
l1:
  cpx $99                               ;device# in
  bcs l2
  jsr JIF_UNTALK
l2:
  jmp $f403                             ;std. ClrAll
.endproc

#endif
