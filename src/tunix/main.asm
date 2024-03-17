 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;                                ;;;
 ;;; ###### ##  ## ####   ## ##  ## ;;;
 ;;;   ##   ##  ## ##  ## ##   ##   ;;;
 ;;;   ##   ###### ##  ## ## ##  ## ;;;
 ;;;                                ;;;
 ;;; Multi-tasking KERNAL extension ;;;
 ;;;  (Commodore VIC-20 + UltiMem)  ;;;
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
FNLEN       = $b7
LFN         = $b8
SA          = $b9
DEV         = $ba
FNADR       = $bb

IOVECTORS   = $031a

IDX_OPEN   = 0
IDX_CLOSE  = 2
IDX_CHKIN  = 4
IDX_CKOUT  = 6
IDX_CLRCN  = 7
IDX_BASIN  = 8
IDX_BSOUT  = 9
IDX_STOP   = 10
IDX_GETIN  = 12
IDX_CLALL  = 14
IDX_USRCMD = 16
IDX_LOAD   = 18
IDX_SAVE   = 20
IDX_BLKIN  = 22
IDX_BKOUT  = 24

;;; BASIC

PRTSTR      = $cb1e

MAX_LFNS    = 256   ; Has to be.
MAX_PROCS   = 64
MAX_DRVS    = 16
MAX_DEVS    = 32

;;; UltiMem

MAX_BANKS   = 128
FIRST_BANK  = 14
ram123      = $9ff4
io23        = $9ff6
blk1        = $9ff8
blk2        = $9ffa
blk3        = $9ffc
blk5        = $9ffe

    .zeropage

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

tmp1:
tmp1l:  .res 1
tmp1h:  .res 1
tmp2:   .res 2
tmp3:   .res 2
tmp4:   .res 2

ptr1:   .res 2
ptr2:   .res 2
ptr3:   .res 2
ptr4:   .res 2

    .data

;;;;;;;;;;;;;;
;;; GLOBAL ;;;
;;;;;;;;;;;;;;

;; Extended memory banks
; List of free ones.
banks:          .res MAX_BANKS
; Number of processes that own a single
; bank.
bank_refs:      .res MAX_BANKS

;; Global logical file numbers
;; Shared by fork()ed processes.
; List of free GLFNs
glfns:          .res MAX_LFNS
; Driver used with GLFN @ OPEN.
glfn_drv:       .res MAX_LFNS
; Secondary address of GLFN @ OPEN.
glfn_sa:        .res MAX_LFNS

;; Processes
; Free slots
procs:          .res MAX_PROCS
; Sleeping/running?
proc_flags:     .res MAX_PROCS
; Primary banks allocated.
proc_lowmem:    .res MAX_PROCS
proc_blk1:      .res MAX_PROCS
proc_blk2:      .res MAX_PROCS
proc_blk3:      .res MAX_PROCS
proc_io23:      .res MAX_PROCS
proc_blk5:      .res MAX_PROCS

;; Drivers
; Free driver slots
drvs:           .res MAX_DRVS
; Processes registered
drv_pid:        .res MAX_DRVS
; Vector tables
drv_vl:         .res MAX_DRVS
drv_vh:         .res MAX_DRVS

; Drivers assigned to devices.
dev_drv:        .res MAX_DEVS

;; Bank allocation
free_bank:      .res 1
first_bank:     .res 1

;; First speed code BLK5 to copy from
;; BLK2 to BLK3.
copy_bank:      .res 1

;; Pointers into array 'proc'.
free_proc:      .res 1
running:        .res 1
sleeping:       .res 1

    .code

;;;;;;;;;;;;;;
;;; MACROS ;;;
;;;;;;;;;;;;;;

.macro mvb to, from
    lda from
    sta to
.endmacro

;;;;;;;;;;;;;;;;;;;
;;; WORD MACROS ;;;
;;;;;;;;;;;;;;;;;;;

.macro ldaxi val
    lda #<val
    ldx #>val
.endmacro

.macro ldayi val
    lda #<val
    ldy #>val
.endmacro

.macro stax to
    sta to
    stx to+1
.endmacro

.macro setzwi to, val
    mvb to, #<val
    mvb to+1, #>val
.endmacro

.macro inczw at
    inc at
    bne :+
    inc at+1
:
.endmacro

.macro qdeczw at
    dec at
    bne :+
    dec at+1
:
.endmacro

.macro jqdeczw at, to
    dec at
    bne to
    dec at+1
    bne to
.endmacro

;;;;;;;;;;;;;;;;;;;
;;; LIST MACROS ;;;
;;;;;;;;;;;;;;;;;;;

.macro list_init list
    lda #1
    sta list
.endmacro

.macro list_popx list, free
    ldx free
    lda list,x
    sta free
    lda #0
    sta list,x
.endmacro

.macro list_popy list, free
    ldy free
    lda list,y
    sta free
    lda #0
    sta list,y
.endmacro

.macro list_pushx list, first
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
    ;; Check head.
    cpx first
    bne :+
    list_popx list, first
    jmp :++++
:   stx tmp2

    ;; Search through next.
    ldy first
:   lda tmp2
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

;;; List of free GLFNS.

.macro pushy_glfn
    list_pushy glfns, glfns
.endmacro

.macro popx_glfn
    list_popx glfns, glfns
.endmacro

.macro popy_glfn
    list_popy glfns, glfns
.endmacro

;;; UI

.macro print asciiz
    lda #<asciiz
    ldy #>asciiz
    jsr PRTSTR
.endmacro

.macro error asciiz
    print asciiz
    ;jmp *
.endmacro

;;;;;;;;;;;;
;;; INIT ;;;
;;;;;;;;;;;;

.proc main
    print txt_tests
    jsr tests
    print txt_booting
    jmp init
.endproc

.export tests
.export banks
.export free_bank
.export first_bank
.proc tests
    jsr init

    ;;; Data structures

    ; Draw GLFNs until empty.
    ldy #1
:   popx_glfn
    stx tmp2
    cpy tmp2
    beq :+
    error err_invalid_glfn_order
:   iny
    bne :--

    ;; Doubly used list arrays.
    ; Allocate free, put back as used.
    list_popx banks, free_bank
    cpx #$1c
    beq :+
    error err_invalid_first_free_bank
:   list_pushx banks, first_bank
    ldaxi banks
    ldy free_bank
    jsr list_length
    cpx #$e3
    beq :+
    error err_fail
:   ldaxi banks
    ldy first_bank
    jsr list_length
    cpx #1
    beq :+
    error err_fail
    ; In reverse.
:   list_popx banks, first_bank
    list_pushx banks, free_bank
    ldaxi banks
    ldy free_bank
    jsr list_length
    cpx #$e4
    beq :+
    error err_fail
:   ldaxi banks
    ldy first_bank
    jsr list_length
    cpx #0
    beq :+
    error err_fail
:
 
    ;; Doubly-linked listmaps.
    ; Allocate first.
    ; Draw till empty.
    ; Free by index.

    ;;; Syscalls

    ;; Extended memory.
    ;; Fork

    rts
.endproc

txt_tests:
    .byte 147   ; Clear screen.
    .byte "TUNIX", 13
    .byte "TESTS..",0
txt_booting:
    .byte 13, "BOOTING..",0
txt_init:
    .byte ".", 0

err_invalid_glfn_order:
    .byte "INVALID GLFN ORDER", 0
err_invalid_first_free_bank:
    .byte "INVALID FIRST FREE BANK", 0
err_fail:
    .byte "TEST FAILED", 0

.proc init
    ;; All banks are R/W RAM.
    ;; Default order expected.
    lda #%11111111
    sta $9ff1
    sta $9ff2

    ;; Clear per-process data.
    setzwi d, $9800
    setzwi c, $07f0
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
    bcs :+
    sta procs,x
:   cpx #MAX_DRVS
    bcs :+
    sta drvs,x
:   inx
    bne :---

    lda #FIRST_BANK
    sta free_bank
    list_init glfns
    list_init drvs
    ; Manually end lists that do not
    ; fill a page.
    lda #0
    sta procs+MAX_PROCS-1
    sta drvs+MAX_DRVS-1

    ;; Save initial set of banks.
    mvb proc_lowmem, ram123
    mvb proc_io23, io23
    mvb proc_blk1, blk1
    mvb proc_blk2, blk2
    mvb proc_blk3, blk3
    mvb proc_blk5, blk5

    ;; Point devices to KERNAL.
    mvb drv_vl, #$1a
    mvb drv_vh, #$03

    ;; Escape into a parallel universe.
    jsr gen_speedcode
    print txt_init
    ldy #0
    jmp fork_raw
.endproc

;;;;;;;;;;;;;;;;;;
;;; SPEED COPY ;;;
;;;;;;;;;;;;;;;;;;

.proc _out
    ldy #0
    sta (d),y
    inc dl
    bne :+
    inc dh
:   rts
.endproc

.macro out val
    lda val
    jsr _out
.endmacro

.macro outzw at
    lda at
    jsr _out
    lda at+1
    jsr _out
.endmacro

.proc gen_speedcode
    ; Grab a new bank for BLK5.
    jsr balloc
    sta copy_bank
    sta blk5
    ; Source/dest argument values.
    setzwi ptr1, $6000
    setzwi ptr2, $a000
    ; Total move count.
    setzwi c, $2100

next_bank:
    ; Per bank move count.
    setzwi tmp1, ($2000 / 6)
    ; Bank fill pointer.
    setzwi d, $a000

next_move:
    ;; Make move.
    out #OP_LDA_ABS
    outzw ptr1
    out #OP_STA_ABS
    outzw ptr2

    ;; Step
    ; Increment argument values.
    inczw ptr1
    inczw ptr2
    ; Decrement total move count.
    qdeczw c
    beq done
    ; Decrement per bank move count.
    jqdeczw tmp1, next_move

    ;; Make switch to next bank.
    out #OP_LDA_IMM
    jsr balloc
    pha
    jsr _out
    out #OP_JMP_ABS
    out #<next_copy_bank
    out #>next_copy_bank
    pla
    sta blk5
    jmp next_bank

done:
    out #OP_RTS
    rts
.endproc

.proc copy_blk3_to_blk5
    stx blk3
    lda copy_bank
.endproc

.proc next_copy_bank
    sta $9ffa
    jmp $4000
.endproc

;;;;;;;;;;;;;;;;;
;;; PROCESSES ;;;
;;;;;;;;;;;;;;;;;

.proc fork
    ;; Grab process slot.
    list_popy procs, free_proc
    beq no_more_procs

    ;; Insert past current process.
    ldx pid
    lda running,x
    sta running,y
    tya
    sta running,x
    pha

    jsr fork_raw

    ;; Increment banks.
    ldy first_lbank
    beq :++
:   ldx banks,y
    inc bank_refs,x
    lda banks,y
    tay
    bne :-
:

    ;; Return PID.
    pla
    ; 0 for parent.
    cmp pid
    bne :+
    lda #0

:   clc
    rts

no_more_procs:
    sec
    rts
.endproc

; fork() copy vectors
set_lowmem: .word $0000, $b000, $0400
set_ram123: .word $0400, $a000, $0c00
set_screen: .word $1000, $a000, $1000
set_color:  .word $9400, $b400, $0400
set_io23:   .word $9800, $b800, $07f0
set_vic:
    .word $9000, saved_vic+$2000, $0010

.proc save_state
    lda proc_lowmem,y
    sta blk5
    ldaxi set_lowmem
    jsr smemcpy
    ldaxi set_screen
    jsr smemcpy
    ldaxi set_color
    jmp smemcpy
.endproc

set_blk5_to_lowmem:
    .word $b000, $0000, $0400
set_blk5_to_screen:
    .word $8000, $1000, $1000
set_blk5_to_color:
    .word $b400, $9400, $0400
set_blk5_to_vic:
    .word saved_vic+$2000, $9000, $0010

.proc load_state
    lda proc_lowmem,y
    sta blk3
    ldx stack-$2000
    txs
    ldaxi set_blk5_to_lowmem
    jsr smemcpy
    ldaxi set_blk5_to_vic
    jsr smemcpy
    ldaxi set_blk5_to_color
    jsr smemcpy
    ldaxi set_blk5_to_screen
    jmp smemcpy
.endproc

; Copy process to new banks.
.proc fork_raw
    lda #0
    sta proc_flags,y

    jsr balloc
    sta proc_lowmem,y
    jsr save_state

    jsr balloc
    sta proc_io23,y
    sta blk5
    ldaxi set_io23
    jsr smemcpy
    sty pid

    jsr balloc
    sta proc_blk1,y
    sta blk5
    ldx blk1
    jsr copy_blk3_to_blk5

    jsr balloc
    sta proc_blk2,y
    sta blk5
    ldx blk2
    jsr copy_blk3_to_blk5

    jsr balloc
    sta proc_blk3,y
    sta blk5
    ldx blk3
    jsr copy_blk3_to_blk5

    jsr balloc
    sta proc_blk5,y
    ldx blk5
    sta blk5
    jsr copy_blk3_to_blk5

    ldx pid

    ;; Release parent's banks.
    lda #0
    ldy proc_lowmem,x
    sta lbanks,y
    ldy proc_io23,x
    sta lbanks,y
    ldy proc_blk1,x
    sta lbanks,y
    ldy proc_blk2,x
    sta lbanks,y
    sty blk2
    ldy proc_blk3,x
    sta lbanks,y
    sty blk3
    ldy proc_blk5,x
    sta lbanks,y
    sty blk5

    ;; Restore parent's banks.
    lda proc_blk2,x
    sta blk3
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
    ; Take off running or sleeping.
    tax
    lda proc_flags,x
    bmi :+
    list_rm procs, running
    jmp :++
:   list_rm procs, sleeping

    ; Add to free.
:   list_pushx procs, free_proc
    clc
    rts
.endproc

.proc switch_to
    pha
    ldy pid

    ;;; Save state.
    ;; Banks
    lda io23
    sta proc_io23,y
    lda blk1
    sta proc_blk1,y
    lda blk2
    sta proc_blk2,y
    lda blk3
    sta proc_blk3,y
    lda blk5
    sta proc_blk5,y
    jsr save_state
    tsx
    inx
    inx
    stx stack-$2000

    ;; Load state.
    pla
    sta pid
    tay
    jsr load_state
    lda proc_lowmem,y
    sta ram123
    lda proc_io23,y
    sta io23
    lda proc_blk1,y
    sta blk1
    lda proc_blk2,y
    sta blk2
    lda proc_blk3,y
    sta blk3
    lda proc_blk5,y
    sta blk5
    ldx stack-$2000
    txs
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
    list_popx banks, free_bank
    beq :+  ; Oops…
    ;; Own it.
    inc bank_refs,x
    inc lbanks,x
:   txa
    rts
.endproc

; Free bank
;
; Ingnores already free ones.
; X: Bank #
.proc bfree
    dec lbanks,x
    bmi invalid_bank
    dec bank_refs,x
    bne :+
    list_pushx banks, free_bank
:   clc
    rts
invalid_bank:
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
    pushy_glfn
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
; Returns: X: driver ID.
;             0 if out of slots.
.proc register_driver
    sta ptr1
    stx ptr1+1

    ;; Get slot.
    list_popx drvs, drvs
    beq out_of_slots

    ;; Populate slot.
    lda pid
    sta drv_pid,x
    lda ptr1
    sta drv_vl,x
    lda ptr1+1
    sta drv_vh,x

out_of_slots:
    rts
.endproc

;;;;;;;;;;;;;;;;;;
;;; LIST UTILS ;;;
;;;;;;;;;;;;;;;;;;

.proc list_length
    stax ptr1
    ldx #0
    cpy #0
    beq empty
:   inx
    lda (ptr1),y
    tay
    bne :-
empty:
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
    ldx cl
    inx
    inc ch
    ldy dl
    lda #0
    sta dl
    bne +n ; (jmp)
l:  sta (d),y
    iny
    beq m
n:  dex
    bne l
    dec ch
    bne l
    rts
m:  inc dh
    jmp n
.endproc

;;;;;;;;;;;;;;;;;
;;; DISPATCH ;;;;
;;;;;;;;;;;;;;;;;

; Translate local to global LFN.
.proc lfn_to_glfn
    ;; Use existing.
    tax
    lda lfn_glfn,x
    bne :+

    ;; Add LFN .
    list_pushx lfns, first_lfn
    beq :+

    ;; Allocate GLFN.
    popy_glfn
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

tunix:
    .word open, chkin, ckout, basin
    .word bsout, getin, clrcn, close
    .word clall, stop, usrcmd, load
    .word save, blkin, bkout

.proc open
    ;; Save LFN and file name.
    lda FNADR
    pha
    lda FNADR+1
    pha
    ldx LFN
    txa
    pha

    jsr lfn_to_glfn
    sta LFN

    ;; Copy file name.
    ldy FNLEN
    beq :++
:   lda (FNADR),y
    sta filename,y
    dey
    jmp :-
:   setzwi FNADR, filename

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
    lda #IDX_OPEN
    jsr call_driver
    sta reg_a

    ;; Restore LFN and file name.
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
    jsr lfn_to_glfn
    sta reg_a
    lda #IDX_CHKIN
    jmp call_driver
.endproc

.proc ckout
    jsr lfn_to_glfn
    sta reg_a
    lda #IDX_CKOUT
    jmp call_driver
.endproc

.macro iohandler name, lfn, drvop
.proc name
    sta reg_a
    stx reg_x
    sty reg_y

    ;; Translate input LFN.
    lda lfn
    pha
    jsr lfn_to_glfn
    sta lfn

    lda #drvop
    jsr call_driver
    sta reg_a

    ;; Restore LFN.
    pla
    sta lfn
    php
    lda reg_a
    plp
    rts
.endproc
.endmacro

iohandler basin, DFLTN, IDX_BASIN
iohandler bsout, DFLTO, IDX_BSOUT
iohandler getin, DFLTN, IDX_GETIN
iohandler blkin, DFLTN, IDX_BLKIN
iohandler bkout, DFLTO, IDX_BKOUT

.proc clrcn
    jsr lfn_to_glfn
    sta reg_a
    ldy glfn_drv,x
    tya
    tax
    lda #IDX_CLRCN
    jmp call_driver
.endproc

.proc close
    jsr lfn_to_glfn
    sta reg_a
    ldy glfn_drv,x
    lda #0
    sta glfn_drv,x
    tya
    tax
    lda #IDX_CLOSE
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

.proc stop
    ldx #0
    lda #IDX_STOP
    jmp call_driver
.endproc

.proc load
    sta reg_a
    stx reg_x
    sty reg_y
    ldy DEV
    ldx dev_drv,y
    lda #IDX_LOAD
    jmp call_driver
.endproc

.proc save
    sta reg_a
    stx reg_x
    sty reg_y
    ldy DEV
    ldx dev_drv,y
    lda #IDX_SAVE
    jmp call_driver
.endproc

.proc usrcmd
    ldx #0
    lda #IDX_STOP
    jmp call_driver
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOCAL (per process) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

    .bss
    .org $9800

;; Extended memory banks
; List of used ones.
lbanks:     .res MAX_BANKS

;; Logical file numbers
; List of used ones
lfns:       .res MAX_LFNS
; Translations to global LFNs
lfn_glfn:   .res MAX_LFNS

;; Process info
pid:        .res 1
; CPU state
reg_a:      .res 1
reg_x:      .res 1
reg_y:      .res 1
stack:      .res 1
flags:      .res 1
; VIC
saved_vic:  .res 16

;; Driver info
; File name copied from calling process.
filename:   .res 256
; Translated LFN.
glfn:       .res 1

first_lfn:  .res 1
first_lbank:.res 1
