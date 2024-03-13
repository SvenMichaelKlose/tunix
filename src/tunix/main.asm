;;; CPU

OP_LDA_IMM  = $a9
OP_LDA_ABS  = $ad
OP_STA_ABS  = $8d
OP_JMP_ABS  = $4c
OP_RTS      = $60

;;; KERNAL

DFLTN       = $99
DFLTO       = $9a
LFN         = $b8   ; Logical File Number
SA          = $b9
DEV         = $ba
IOPEN       = $031a

;;; BASIC

PRTSTR      = $cb1e ; Print ASCIIZ string,

MAX_LFNS    = 256   ; Has to be.
MAX_PROCS   = 64
MAX_DRVS    = 16
MAX_DEVS    = 32

;;; UltiMem

MAX_BANKS   = 128   ; UltiMem RAM banks.
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

procs:          .res MAX_PROCS
running:        .res MAX_PROCS
proc_lowmem:    .res MAX_PROCS
proc_screen:    .res MAX_PROCS
proc_blk1:      .res MAX_PROCS
proc_blk2:      .res MAX_PROCS
proc_blk3:      .res MAX_PROCS
proc_io23:      .res MAX_PROCS
proc_blk5:      .res MAX_PROCS

drvs:           .res MAX_DRVS
drv_pid:        .res MAX_DRVS
drv_vl:         .res MAX_DRVS
drv_vh:         .res MAX_DRVS

dev_drv:        .res MAX_DEVS

free_bank:      .res 1
copy_bank:      .res 1
free_proc:      .res 1
first_running:  .res 1
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
:   inx
    bne :--

    lda #FIRST_BANK
    sta free_bank

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
    jmp fork_raw
:   rts
.endproc

set_lowmem: .word $0000, $a000, $1000
set_screen: .word $1000, $a000, $1000
set_blk1:   .word $2000, $a000, $2000
set_blk2:   .word $4000, $a000, $2000
set_blk3:   .word $6000, $a000, $2000
set_io23:   .word $9800, $b800, $0800

;; Copy process to newly allocated banks.
.proc fork_raw
    jsr balloc
    sta proc_lowmem,y
    sta blk5
    lda #<set_lowmem
    ldx #>set_lowmem
    jsr smemcpy

    jsr balloc
    sta proc_screen,y
    sta blk5
    lda #<set_screen
    ldx #>set_screen
    jsr smemcpy

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

    jsr balloc
    sta proc_io23,y
    sta blk5
    lda #<set_io23
    ldx #>set_io23
    jsr smemcpy
    sty pid

    lda blk5
    sta blk3
    jsr balloc
    sta proc_blk5,y
    sta blk5
    lda #<set_blk3
    ldx #>set_blk3
    jsr smemcpy

    ldx pid

    ;; Restore banks.
    lda proc_blk3,x
    sta blk3
    lda proc_blk5,x
    sta blk5

    ;; Un-inherit parent's address space.
    lda #0
    ldy proc_lowmem,x
    sta lbanks,y
    ldy proc_screen,x
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

    rts
.endproc

.proc taskswitch
    ;; Save stack reg.
    ;; Save bank config.
    ;; Save lowmem.
    ;; Save VIC.
    ;; Save screen.
    ;; Load lowmem.
    ;; Load VIC.
    ;; Load screen.
    ;; Load bank config.
    ;; Load stack reg.
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;
;;; EXTENDED MEMORY ;;;
;;;;;;;;;;;;;;;;;;;;;;;

;;; Allocate bank
;;;
;;; Returns:
;;;  Z: Out of memory.
;;;  X: Bank #
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

;;; Free bank
;;;
;;; X: Bank #
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

;;; Free all banks of current process.
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
:   ldy lfn_glfn,x
    list_pushy glfns, free_glfn
    lda lfns,x
    tax
    bne :-
done:
    rts
.endproc

;;;;;;;;;;;;;;;
;;; DRIVERS ;;;
;;;;;;;;;;;;;;;

;;; XA: vectors
;;; Returns: X: driver ID. 0 on error.
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

;;; Init s, d and c with values at XA.
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

.proc smemcpy
    jsr sset
.endproc

;;; Copy memory.
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

;;; Permanently ranslate local to global LFN.
.proc xlat_lfn_glfn
    lda lfn_glfn,x
    bne :+
    list_pop lfns, free_lfn
    list_push lfns, first_lfn
    beq :+
    lda LFN
    sta lfn_id,x
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
    sta ptr1
    lda drv_vh,y
    adc #0
    sta ptr1+1

    ;; Push banks.
    lda blk1
    pha

    ;; Bank in driver.
    ldx drv_pid,y
    lda proc_blk1,x
    sta blk1

    ;; Call with registers.
    lda reg_a
    ldx reg_x
    ldy reg_y
    jsr +j

    ;; Pop banks.
    pla
    sta blk1

    rts

j:  jmp (ptr1)
.endproc

.proc open
    ;; Get GLFN.
    ldx LFN
    jsr xlat_lfn_glfn

    ;; Assign driver to GLFN.
    ldy DEV
    lda dev_drv,y
    sta glfn_drv,x

    ;; Call.
    tax
    lda #0
    jmp call_driver
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
    lda DFLTN
    pha
    tax
    jsr xlat_lfn_glfn
    stx DFLTN
    lda #6
    jsr call_driver
    sta itmp
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

glfn:       .res 1
pid:        .res 1

free_lfn:   .res 1
first_lfn:  .res 1

reg_a:      .res 1
reg_x:      .res 1
reg_y:      .res 1
reg_s:      .res 1
flags:      .res 1

lbanks:     .res MAX_BANKS
lfns:       .res MAX_LFNS
lfn_id:     .res MAX_LFNS
lfn_glfn:   .res MAX_LFNS
