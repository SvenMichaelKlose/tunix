PRTSTR      = $cb1e

FIRST_BANK  = 6
MAX_BANKS   = 128
MAX_PROCS   = 64

ram123      = $9ff4
io23        = $9ff6
blk1        = $9ff8
blk2        = $9ffa
blk3        = $9ffc
blk5        = $9ffe

    .zeropage

.importzp tmp1, tmp2, tmp3, tmp4
.importzp ptr1, ptr2, ptr3, ptr4

s:
sl:     .res 1
sh:     .res 1
d:
dl:     .res 1
dh:     .res 1
c:
cl:     .res 1
ch:     .res 1

    .bss

;;;;;;;;;;;;;;
;;; GLOBAL ;;;
;;;;;;;;;;;;;;

;; Memory banks

banks:      .res MAX_BANKS
bank_ref:   .res MAX_BANKS
free_bank:  .res 1

copy_bank:  .res 1

;; Processes

procs:        .res MAX_PROCS
running:      .res MAX_PROCS
proc_lowmem:  .res MAX_PROCS
proc_screen:  .res MAX_PROCS
proc_blk1:    .res MAX_PROCS
proc_blk2:    .res MAX_PROCS
proc_blk3:    .res MAX_PROCS
proc_io23:    .res MAX_PROCS
proc_blk5:    .res MAX_PROCS

free_proc:      .res 1
first_running:  .res 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOCAL (per process) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

lbanks: .res MAX_BANKS

pid:    .res 1

    .code

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

.proc init
    ;; All banks are R/W RAM.
    lda #%11111111
    sta $9ff1
    sta $9ff2

    ;; Set up lists and tables.
    ldx #0
:   txa
    clc
    adc #1
    sta banks,x
    cpx #MAX_PROCS
    bcc :+
    sta procs,x
:   lda #0
    sta bank_ref,x
    sta lbanks,x
    inx
    bne :--

    lda #FIRST_BANK
    sta free_bank
    lda #0
    sta free_proc

    jsr gen_copycode

    ;; Make process 0 (which is never running).
    jsr fork_raw

    lda #<txt_welcome
    ldy #>txt_welcome
    jmp PRTSTR
.endproc

OP_LDA_IMM  = 0
OP_LDA_ABS  = 0
OP_STA_ABS  = 0
OP_JMP_ABS  = 0
OP_RTS      = 0

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

:   lda #OP_LDA_ABS
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
    beq :++
:   dec tmp3
    bne :--
    dec tmp4
    bne :--

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

    ;; Make switch to next bank
:   lda #OP_RTS
    jmp out
    sta blk5
.endproc

.proc next_copy_bank
    sta $9ffe
    jmp $a000
.endproc

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

;; Copy process to newly allocated banks.
.proc fork_raw
    jsr balloc
    sta proc_lowmem,y
    sta blk5
    lda #0
    sta sl
    sta sh
    sta dl
    sta cl
    lda #$a0
    sta dh
    lda #$20
    sta ch
    jsr memcpy
    jsr balloc
    sta proc_screen,y
    sta blk5
    lda #0
    sta sl
    sta dl
    sta cl
    lda #$10
    sta sh
    sta ch
    lda #$a0
    sta dh
    jsr memcpy
    jsr balloc
    sta proc_blk1,y
    sta blk5
    lda #0
    sta sl
    sta dl
    sta cl
    lda #$20
    sta sh
    sta ch
    lda #$a0
    sta dh
    jsr memcpy
    jsr balloc
    sta proc_blk2,y
    sta blk5
    lda #0
    sta sl
    sta dl
    sta cl
    lda #$40
    sta sh
    lda #$a0
    sta dh
    lda #$20
    sta ch
    jsr memcpy
    jsr balloc
    sta proc_blk3,y
    sta blk5
    lda #0
    sta sl
    sta dl
    sta cl
    lda #$60
    sta sh
    lda #$a0
    sta dh
    lda #$20
    sta ch
    jsr memcpy
    jsr balloc
    sta proc_io23,y
    sta blk5
    lda #0
    sta sl
    sta dl
    sta cl
    lda #$98
    sta sh
    lda #$b8
    sta dh
    lda #$08
    sta ch
    jsr memcpy
    sty pid+$2000
    jsr balloc
    sta proc_blk5,y
    sta blk3
    lda #0
    sta sl
    sta dl
    sta cl
    lda #$a0
    sta sh
    lda #$60
    sta dh
    lda #$20
    sta ch
    jsr memcpy

    ;; Un-inherit parent's address space.
    ldx pid
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

;;; Copy memory.
.proc memcpy
    ldy #0
    ldx cl
    inx
    inc ch
    bne copy_forwards   ; (jmp)

l:  lda (s),y
    sta (d),y
inc $900f
    iny
    beq k
copy_forwards:
q:  dex
    bne l
    dec ch
    bne l
r:  rts
k:  inc sh
    inc dh
    jmp q
.endproc

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
    list_push banks, free_bank
    clc
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

    .data

txt_welcome:    .byte "TUNIX", 13, 0
