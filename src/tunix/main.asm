;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                :::
;;; ###### ##  ## ####   ## ##  ## ;;;
;;;   ##   ##  ## ##  ## ##   ##   ;;;
;;;   ##   ###### ##  ## ## ##  ## ;;;
;;;                                :::
;;; Multi-tasking KERNAL extension ;;;
;;;  (Commodore VIC-20 + UltiMem)  :::
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CPU

OP_LDA_IMM  = $a9
OP_LDA_ABS  = $ad
OP_STA_ABS  = $8d
OP_JMP_ABS  = $4c
OP_RTS      = $60

;;; KERNAL

DFLTN       = $99
DFLTO       = $9a
FNLEN       = $b8
LFN         = $b8
SA          = $b9
DEV         = $ba
FNADR       = $bb
IOPEN       = $031a

;;; BASIC

PRTSTR      = $cb1e

MAX_LFNS    = 256   ; Has to be.
MAX_PROCS   = 64
MAX_DRVS    = 16
MAX_DEVS    = 32

;;; UltiMem

MAX_BANKS   = 128
FIRST_BANK  = 6
ram123      = $9ff4
io23        = $9ff6
blk1        = $9ff8
blk2        = $9ffa
blk3        = $9ffc
blk5        = $9ffe

    .zeropage

.importzp tmp1, tmp2, tmp3, tmp4
.importzp ptr1, ptr2, ptr3, ptr4

;;; Registers

s:
sl:     .res 1
sh:     .res 1
d:
dl:     .res 1
dh:     .res 1
c:
cl:     .res 1
ch:     .res 1

    .data

;;;;;;;;;;;;;;
;;; GLOBAL ;;;
;;;;;;;;;;;;;;

banks:          .res MAX_BANKS
bank_ref:       .res MAX_BANKS

glfns:          .res MAX_LFNS
glfn_drv:       .res MAX_LFNS
glfn_sa:        .res MAX_LFNS

procs:          .res MAX_PROCS
proc_flags:     .res MAX_PROCS
proc_lowmem:    .res MAX_PROCS
proc_blk1:      .res MAX_PROCS
proc_blk2:      .res MAX_PROCS
proc_blk3:      .res MAX_PROCS
proc_io23:      .res MAX_PROCS
proc_blk5:      .res MAX_PROCS

drvs:           .res MAX_DRVS
drv_pid:        .res MAX_DRVS
drv_vl:         .res MAX_DRVS
drv_vh:         .res MAX_DRVS

devs:           .res MAX_DEVS
dev_drv:        .res MAX_DEVS

free_bank:      .res 1
copy_bank:      .res 1
free_proc:      .res 1
running:        .res 1
sleeping:       .res 1
free_glfn:      .res 1
free_drv:       .res 1

    .code

;;;;;;;;;;;;;;;;;;;
;;; LIST MACROS ;;;
;;;;;;;;;;;;;;;;;;;

.macro list_pop list, free
    ldx free
    lda list,x
    sta free
.endmacro

.macro list_popy list, free
    ldy free
    lda list,y
    sta free
.endmacro

.macro list_push list, first
    lda first
    sta list,x
    stx first
.endmacro

.macro list_pushy list, first
    lda first
    sta list,y
    sty first
.endmacro

.macro list_rm list, first
    ;; Handle head.
    cpx first
    bne :+
    list_pop list, first
    jmp :++++
:   stx tmp1

    ;; Search through next.
    ldy first
:   lda tmp1
    cmp list,y
    bne :+
    ldx list,y
    lda list,x
    sta list,y
    jmp :++
:   lda list,y
    tay
    bne :--
:
.endmacro

;;;;;;;;;;;;
;;; INIT ;;;
;;;;;;;;;;;;

.proc main
    ;; All banks are R/W RAM.
    ;; Default order expected.
    lda #%11111111
    sta $9ff1
    sta $9ff2

    ;; Clear per-process data.
    lda #$00
    sta dl
    lda #$98
    sta dh
    lda #$f0
    sta cl
    lda #$07
    sta ch
    jsr bzero

    ;; Set up lists and tables.
    ldx #0
:   txa
    clc
    adc #1
    sta banks,x
    sta glfns,x
    sta lfns,x
    cpx #MAX_PROCS
    bcc :+
    sta procs,x
:   cpx #MAX_DRVS
    bcc :+
    sta drvs,x
:   cpx #MAX_DEVS
    bcc :+
    sta devs,x
:   inx
    bne :----

    lda #FIRST_BANK
    sta free_bank
    lda #1
    sta free_glfn

    ;; Save initial set of banks.
    lda ram123
    sta proc_lowmem
    lda io23
    sta proc_io23
    lda blk1
    sta proc_blk1
    lda blk2
    sta proc_blk2
    lda blk3
    sta proc_blk3
    lda blk5
    sta proc_blk5

    ;; Point devices to KERNAL.
    lda #$1a
    sta drv_vl
    lda #$03
    sta drv_vh

    ;; Escape into a parallel universe.
    jsr gen_copycode
    ldy #0
    jsr fork_raw

    lda #<txt_welcome
    ldy #>txt_welcome
    jmp PRTSTR
.endproc

;;;;;;;;;;;;;;;;;;
;;; SPEED COPY ;;;
;;;;;;;;;;;;;;;;;;

.proc out
    ldy #0
    sta (d),y
    inc dl
    bne :+
    inc dh
:   rts
.endproc

.proc gen_copycode
    jsr balloc
    sta copy_bank
    lda #$00
    sta ptr1
    sta ptr2
    lda #$60
    sta ptr1+1
    lda #$a0
    sta ptr2+1
    lda #$00+1
    sta tmp1
    lda #$20+1
    sta tmp2
next_bank:
    lda #<1364+1
    sta tmp3
    lda #>1364+1
    sta tmp4
    lda #$00
    sta dl
    lda #$a0
    sta dh
    jmp start

next_move:
    lda #OP_LDA_ABS
    jsr out
    lda ptr1
    jsr out
    lda ptr1+1
    jsr out
    lda #OP_STA_ABS
    jsr out
    lda ptr2
    jsr out
    lda ptr2+1
    jsr out
    inc ptr1
    bne n1
    inc ptr1+1
n1: inc ptr2
    bne start
    inc ptr2+1
start:
    dec tmp1
    bne :+
    dec tmp2
    beq done
:   dec tmp3
    bne next_move
    dec tmp4
    bne next_move

    ;; Make switch to next bank.
    lda #OP_LDA_IMM
    jsr out
    jsr balloc
    pha
    jsr out
    lda #OP_JMP_ABS
    jsr out
    lda #<next_copy_bank
    jsr out
    lda #>next_copy_bank
    jsr out
    pla
    sta blk5
    jmp next_bank

done:
    lda #OP_RTS
    jmp out
    sta blk5
.endproc

.proc next_copy_bank
    sta $9ffe
    jmp $a000
.endproc

;;;;;;;;;;;;;;;;;
;;; PROCESSES ;;;
;;;;;;;;;;;;;;;;;

.proc fork
    list_popy procs, free_proc
    beq :+
    ;; Insert past current process.
    ldx pid
    lda running,x
    sta running,y
    tya
    sta running,x

    pha

    jsr fork_raw
    ;; Increment GLFNS.
    ;; Increment banks.

    pla
    clc
    rts
:   sec
    rts
.endproc

; fork() copy vectors
set_lowmem: .word $0000, $b000, $0400
set_screen: .word $1000, $a000, $1000
set_color:  .word $9400, $b400, $0400
set_vic:    .word $9000, $b800, $0400
set_blk1:   .word $2000, $a000, $2000
set_blk2:   .word $4000, $a000, $2000
set_blk3:   .word $6000, $a000, $2000
set_io23:   .word $9800, $b810, $07f0

; Copy process to new banks.
.proc fork_raw
    lda #0
    sta proc_flags,y

    jsr balloc
    sta proc_lowmem,y
    sta proc_io23,y
    sta blk5
    lda #<set_lowmem
    ldx #>set_lowmem
    jsr smemcpy
    lda #<set_screen
    ldx #>set_screen
    jsr smemcpy
    lda #<set_color
    ldx #>set_color
    jsr smemcpy
    lda #<set_io23
    ldx #>set_io23
    jsr smemcpy
    sty pid

    jsr balloc
    sta proc_blk1,y
    sta blk5
    lda #<set_blk1
    ldx #>set_blk1
    jsr smemcpy

    jsr balloc
    sta proc_blk2,y
    sta blk5
    lda #<set_blk2
    ldx #>set_blk2
    jsr smemcpy

    jsr balloc
    sta proc_blk3,y
    sta blk5
    lda #<set_blk3
    ldx #>set_blk3
    jsr smemcpy

    lda blk5
    sta blk3
    jsr balloc
    sta proc_blk5,y
    sta blk5
    lda #<set_blk3
    ldx #>set_blk3
    jsr smemcpy

    ldx pid

    ;; Release parent's banks.
    lda #0
    ldy proc_lowmem,x
    sta lbanks,y
    ldy proc_blk1,x
    sta lbanks,y
    ldy proc_blk2,x
    sta lbanks,y
    ldy proc_blk3,x
    sta lbanks,y
    ldy proc_io23,x
    sta lbanks,y
    ldy proc_blk5,x
    sta lbanks,y

    ;; Restore banks.
    lda proc_blk3,x
    sta blk3
    lda proc_blk5,x
    sta blk5

    rts
.endproc

; A: Process ID.
.proc kill
    pha

    ;; Close resources.
    ; Switch to context.
    ldx pid
    lda io23
    pha
    lda proc_io23,x
    sta io23
    ; Free.
    jsr clall
    jsr free_lfns
    jsr bprocfree
    ; Restore context.
    pla
    sta io23

    pla

    ;; Free process
    ; Take off list.
    tax
    lda proc_flags,x
    bmi :+
    list_rm procs, running
    beq error
    jmp :++
:   list_rm procs, sleeping
    beq error
:
    ; Add to free.
    list_push procs, free_proc
    clc
    rts
error:
    sec
    rts
.endproc

; Saving state
set_lowmem_to_blk3:
    .word $0000, $7000, $0400
set_vic_to_blk3:
    .word $9000, saved_vic-$2000, $0010
set_screen_to_blk3:
    .word $1000, $6000, $1000
set_color_to_blk3:
    .word $9400, $7400, $0400

; Loading state
set_blk3_to_lowmem:
    .word $7000, $0000, $0400
set_blk3_to_vic:
    .word saved_vic-$2000, $9000, $0010
set_blk3_to_screen:
    .word $6000, $1000, $1000
set_blk3_to_color:
    .word $7400, $9400, $0400

.proc switch_to
    pha
    ldx pid

    ;;; Save state.
    ;; Banks
    lda io23
    sta proc_io23,x
    lda blk1
    sta proc_blk1,x
    lda blk2
    sta proc_blk2,x
    lda blk3
    sta proc_blk3,x
    lda blk5
    sta proc_blk5,x
    ;; Low memory
    ; Zero page, stack, KERNAL
    lda proc_lowmem,x
    sta blk3
    lda #<set_lowmem_to_blk3
    ldx #>set_lowmem_to_blk3
    jsr smemcpy
    ; VIC
    lda #<set_vic_to_blk3
    ldx #>set_vic_to_blk3
    jsr smemcpy
    ; Color
    lda #<set_color_to_blk3
    ldx #>set_color_to_blk3
    jsr smemcpy
    ; Internal 4K
    lda #<set_screen_to_blk3
    ldx #>set_screen_to_blk3
    jsr smemcpy
    tsx
    inx
    stx stack-$2000

    pla
    sta pid
    tax

    ;; Load state.
    lda proc_lowmem,x
    sta blk3
    ldx stack-$2000
    txs
    lda #<set_blk3_to_lowmem
    ldx #>set_blk3_to_lowmem
    jsr smemcpy
    lda #<set_blk3_to_vic
    ldx #>set_blk3_to_vic
    jmp smemcpy
    lda #<set_blk3_to_color
    ldx #>set_blk3_to_color
    jmp smemcpy
    lda #<set_blk3_to_screen
    ldx #>set_blk3_to_screen
    jsr smemcpy
    lda proc_lowmem,x
    sta ram123
    lda proc_io23,x
    sta io23
    lda proc_blk1,x
    sta blk1
    lda proc_blk2,x
    sta blk2
    lda proc_blk3,x
    sta blk3
    lda proc_blk5,x
    sta blk5
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;
;;; EXTENDED MEMORY ;;;
;;;;;;;;;;;;;;;;;;;;;;;

; Allocate bank
;
; Returns:
;  Z: Out of memory.
;  X: Bank #
.proc balloc
    ;; Draw from global pool.
    list_pop banks, free_bank
    beq :+  ; Oops…
    ;; Own it.
    inc bank_ref,x
    inc lbanks,x
:   txa
    rts
.endproc

; Free bank
;
; X: Bank #
.proc bfree
    dec lbanks,x
    bmi error
    dec bank_ref,x
    bne :+
    list_push banks, free_bank
:   clc
    rts
error:
    inc lbanks,x
    sec
    rts
.endproc

; Free all banks of current process.
.proc bprocfree
    ldx #FIRST_BANK
:   lda lbanks,x
    beq :+
    jsr bfree
:   inx
    cpx #MAX_BANKS
    bne :--
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOGICAL FILE NUMBERS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.proc free_lfns
    ldx first_lfn
    beq done
:   dec lfn_glfn,x
    bne :+
    ldy lfn_glfn,x
    list_pushy glfns, free_glfn
:   lda lfns,x
    tax
    bne :--
done:
    rts
.endproc

;;;;;;;;;;;;;;;
;;; DRIVERS ;;;
;;;;;;;;;;;;;;;

; XA: vectors
; Returns: X: driver ID. 0 on error.
.proc register_driver
    sta ptr1
    stx ptr1+1

    ;; Get slot.
    list_pop drvs, free_drv
    beq error

    ;; Populate slot.
    lda pid
    sta drv_pid,x
    lda ptr1
    sta drv_vl,x
    lda ptr1+1
    sta drv_vh,x
error:
    rts
.endproc

;;;;;;;;;;;;;
;;; ZPLIB ;;;
;;;;;;;;;;;;;

; Init s, d and c with values at XA.
.proc sset
    sta p+1
    stx p+2
    ldx #5
p:  lda $ff00,x
    sta 0,x
    dex
    bpl p
    rts
.endproc

;;;;;;;;;;;;;;
;;; STDLIB ;;;
;;;;;;;;;;;;;;

; Copy range at XA.
.proc smemcpy
    jsr sset
.endproc

; Copy memory.
.proc memcpy
    tya
    pha
    ldy #0
    ldx cl
    inx
    inc ch
    bne copy_forwards   ; (jmp)

l:  lda (s),y
    sta (d),y
    iny
    beq k
copy_forwards:
q:  dex
    bne l
    dec ch
    bne l
r:  pla
    tay
    rts
k:  inc sh
    inc dh
    jmp q
.endproc

; Clear memory.
.proc bzero
    ldx c
    inx
    inc c+1
    ldy d
    lda #0
    sta d
    bne +n ; (jmp)
l:  sta (d),y
    iny
    beq m
n:  dex
    bne l
    dec c+1
    bne l
    rts
m:  inc d+1
    jmp n
.endproc

;;;;;;;;;;;;;;;;;
;;; DISPATCH ;;;;
;;;;;;;;;;;;;;;;;

.byte "DISPATCH"

; Translate local to global LFN.
.proc xlat_lfn_glfn
    ;; Use existing.
    lda lfn_glfn,x
    bne :+

    ;; Add LFN .
    list_push lfns, first_lfn
    beq :+

    ;; Allocate GLFN.
    list_popy glfns, free_glfn
    tya
    sta lfn_glfn,x

:   sta glfn
    rts
.endproc

; X: GLFN
; A: vector offset
.proc call_driver
    ;; Get vector (base + A).
    ldy glfn_drv,x
    clc
    adc drv_vl,y
    sta j+1
    lda drv_vh,y
    adc #0
    sta j+2

    ;; Bank in driver.
    lda blk1
    pha
    ldx drv_pid,y
    lda proc_blk1,x
    sta blk1

    ;; Call with registers.
    lda reg_a
    ldx reg_x
    ldy reg_y
j:  jsr $fffe

    ;; Restore bank.
    pla
    sta blk1

    rts
.endproc

.proc open
    lda FNADR
    pha
    lda FNADR+1
    pha
    ldx LFN
    txa
    pha

    ;; Get GLFN.
    jsr xlat_lfn_glfn
    stx LFN

    ;; Copy file name.
    ldy FNLEN
    beq :++
:   lda (FNADR),y
    sta filename,y
    dey
    jmp :-
:   lda #<filename
    sta FNADR
    lda #>filename
    sta FNADR+1

    ;; Assign driver to GLFN.
    ldy DEV
    lda dev_drv,y
    sta glfn_drv,x

    ;; Assign secondary address to GLFN.
    pha
    lda SA
    sta glfn_sa,x
    pla

    ;; Call.
    tax
    lda #0
    jsr call_driver
    sta reg_a

    ;; Restore LFN.
    pla
    sta LFN
    pla
    sta FNADR+1
    pla
    sta FNADR
    php
    lda reg_a
    plp
    rts
.endproc

.proc chkin
    tax
    jsr xlat_lfn_glfn
    stx reg_a
    lda #2
    jmp call_driver
.endproc

.proc ckout
    tax
    jsr xlat_lfn_glfn
    stx reg_a
    lda #4
    jmp call_driver
.endproc

itmp:   .res 1

.proc basin
    ;; Push input LFN.
    lda DFLTN
    pha

    tax
    jsr xlat_lfn_glfn
    stx DFLTN

    lda #6
    jsr call_driver
    sta itmp

    ;; Pop input LFN.
    pla
    sta DFLTN
    php
    lda itmp
    plp

    rts
.endproc

.proc bsout
    sta reg_a

    lda DFLTO
    pha

    tax
    jsr xlat_lfn_glfn
    stx DFLTO

    lda #8
    jsr call_driver
    sta itmp

    ;; Pop output LFN.
    pla
    sta DFLTO
    php
    lda itmp
    plp

    rts
.endproc

.proc close
    tax
    jsr xlat_lfn_glfn
    stx reg_a

    ldy glfn_drv,x
    lda #0
    sta glfn_drv,x

    tya
    tax
    lda #10
    jmp call_driver
.endproc

.proc clall
    ldx first_lfn
    beq r
:   txa
    pha
    jsr close
    pla
    tax
    lda lfns,x
    tax
    bne :-
r:  rts
.endproc

    .data

txt_welcome:    .byte "TUNIX", 13, 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOCAL (per process) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

    .bss
    .org $9800

lbanks:     .res MAX_BANKS
lfns:       .res MAX_LFNS
lfn_glfn:   .res MAX_LFNS
filename:   .res 256

glfn:       .res 1
pid:        .res 1

first_lfn:  .res 1

reg_a:      .res 1
reg_x:      .res 1
reg_y:      .res 1
stack:      .res 1
flags:      .res 1
saved_vic:  .res 16
