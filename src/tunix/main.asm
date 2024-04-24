 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;                                ;;;
 ;;; ###### ##  ## ####   ## ##  ## ;;;
 ;;;   ##   ##  ## ##  ## ##   ##   ;;;
 ;;;   ##   ###### ##  ## ## ##  ## ;;;
 ;;;                                ;;;
 ;;; Multi-tasking KERNAL extension ;;;
 ;;;  (Commodore VIC-20 + UltiMem)  ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.export _tunix = boot
.import __STARTUP_RUN__

    .segment "PRGSTART"

    jmp __STARTUP_RUN__

.export debug
.proc debug
    rts
.endproc

;;; Compile-time

__VIC20__       = 1
START_INIT      = 1
EARLY_TESTS     = 1
BLEEDING_EDGE   = 1

;; Assertions

; IO23, RAM123 * 2, BLK1/2/3/5
DEFAULT_BANKS_PER_PROC  = 7

;;; Segmentation

.import __TESTS_LOAD__
.import __TESTS_RUN__
.import __TESTS_SIZE__
.import __GLOBALBSS_RUN__
.import __GLOBALBSS_SIZE__
.import __LOCALCODE_LOAD__
.import __LOCALCODE_RUN__
.import __LOCALCODE_SIZE__
.import __LOCALBSS_RUN__
.import __LOCALBSS_SIZE__
.import __LOCALBSS2_RUN__
.import __LOCALBSS2_SIZE__
.import __ULTIMEM_SIZE__

;;; Zeropage utils

.import zpw_dec_x
.import zpw_add_xy
.import zpw_cmp_xy

;;; Syscall wrappers

.import lib_schedule
.import lib_getpid
.import lib_fork
.import lib_exit
.import lib_kill
.import lib_wait
.import lib_iopage_alloc
.import lib_iopage_commit
.import lib_iopage_free
.import lib_proc_list
.import lib_proc_info

;;; CPU

OP_LDA_IMM  = $a9
OP_LDA_ABS  = $ad
OP_STA_ABS  = $8d
OP_JMP_ABS  = $4c
OP_RTS      = $60

;;; UltiMem

.export ram123, io23, blk1, blk2, blk3
.export blk5

MAX_BANKS   = 128
FIRST_BANK  = 8

    .segment "ULTIMEM"

ulticfg0: .res 1
ulticfg1: .res 1
ulticfg2: .res 1
ultiid:   .res 1
ram123:   .res 2
io23:     .res 2
blk1:     .res 2
blk2:     .res 2
blk3:     .res 2
blk5:     .res 2

;;; KERNAL

.include "cbm_kernal.inc"

.export FRESTOR

PETSCII_CLRSCR = 147

INITVIC = $e5c3
FRESTOR = $fd52

STATUS  = $90
DFLTN   = $99
DFLTO   = $9a
FNLEN   = $b7
LFN     = $b8
SA      = $b9
DEV     = $ba
FNADR   = $bb
NDX     = $c6 ; keyboard buffer length
KEYD    = $0277


IOVECTORS       = $031a
IOVECTOR_SIZE   = 30

IDX_OPEN   = 0
IDX_CLOSE  = 2
IDX_CHKIN  = 4
IDX_CKOUT  = 6
IDX_CLRCHN = 8
IDX_BASIN  = 10
IDX_BSOUT  = 12
IDX_STOP   = 14
IDX_GETIN  = 16
IDX_CLALL  = 18
IDX_USRCMD = 20
IDX_LOAD   = 22
IDX_SAVE   = 24
IDX_BLKIN  = 26
IDX_BKOUT  = 28

;;; BASIC

TOK_LOAD   = $93

TXTTAB     = $2b

RAMSPC     = $c408
READY      = $c474
PRTSTR     = $cb1e
PRTFIX     = $ddcd
COLDBA     = $e378
INITBA     = $e3a4
INITVCTRS  = $e45b

;;; TUNIX

TUNIX_DEVICE = 31
MAX_LFNS     = 256  ; Has to be.
MAX_PROCS    = 64   ; No more than 127.
MAX_SIGNALS  = 64
MAX_DRVS     = 16
MAX_DEVS     = 32
;MAX_DRV_NAME = 8
MAX_IOPAGES  = 4
IOPAGE_BASE  = $9b
STACK_FENCE  = 16

;;;;;;;;;;;;;;;;
;;; ZEROPAGE ;;;
;;;;;;;;;;;;;;;;
;
; To be restored on syscall return.

    .zeropage

.export s, sl, sh, d, dl, dh
.export c, cl, ch
.export zp1, zp2

s:
sl:     .res 1
sh:     .res 1
d:
dl:     .res 1
dh:     .res 1
c:
cl:     .res 1
ch:     .res 1

zp1:    .res 2
zp2:    .res 2

;;;;;;;;;;;;;;
;;; GLOBAL ;;;
;;;;;;;;;;;;;;
;
; The same for all processes.

    .segment "GLOBALBSS"

.export banks, free_bank
.export bank_refs, iopages, iopagesb
.export free_iopage, first_iopage
.export iopage_pid, iopage_page, glfns
.export glfn_dev
.export glfn_refs, glfn_drv, procs
.export procsb, free_proc, running
.export sleeping, zombie, proc_flags
.export exit_codes, proc_ram123
.export proc_data, proc_io23, proc_blk1
.export proc_blk2, proc_blk3, proc_blk5
.export drvs, drv_pid, drv_dev, drv_vl
.export drv_vh, dev_drv
.export banks_ok, banks_faulty
.export speedcopy_blk3_to_blk5
.export speedcopy_blk5_to_lowmem
.export speedcopy_lowmem_to_blk5

;; Temporaries
tmp1:   .res 2
tmp2:   .res 2
tmp3:   .res 2
ptr1:   .res 2

; TODO: Check what that +1 thing with
; array sizes was about.

;;; Speed code banks
speedcopy_blk3_to_blk5:   .res 1
speedcopy_blk5_to_lowmem: .res 1
speedcopy_lowmem_to_blk5: .res 1

;;; Extended memory banks
banks_ok:       .res 1
banks_faulty:   .res 1
banks:          .res MAX_BANKS + 1
free_bank:      .res 1
bank_refs:      .res MAX_BANKS + 1

;;; IO pages
iopages:        .res MAX_IOPAGES + 1
iopagesb:       .res MAX_IOPAGES + 1
free_iopage:    .res 1
first_iopage:   .res 1
iopage_pid:     .res MAX_IOPAGES + 1
iopage_page:    .res MAX_IOPAGES + 1

;;; Global logical file numbers
;;; Shared by fork()ed processes.
;; Free list
glfns:          .res MAX_LFNS
glfn_refs:      .res MAX_LFNS
free_glfn:      .res 1
;; Last parameters to OPEN.
glfn_drv:       .res MAX_LFNS
glfn_dev:       .res MAX_LFNS
glfn_sa:        .res MAX_LFNS

;;; Processes
procs:          .res MAX_PROCS
procsb:         .res MAX_PROCS
free_proc:      .res 1
running:        .res 1
sleeping:       .res 1
zombie:         .res 1
PROC_BABY       = $10
PROC_ZOMBIE     = $20
PROC_RUNNING    = $40
PROC_SLEEPING   = $80
proc_flags:     .res MAX_PROCS
exit_codes:     .res MAX_PROCS
;; Current banks.
; Kernel and IO pages (per-process)
proc_io23:      .res MAX_PROCS
; Shadow RAM123 (per-process)
proc_data:      .res MAX_PROCS
; Process RAM123
proc_ram123:    .res MAX_PROCS
proc_blk1:      .res MAX_PROCS
proc_blk2:      .res MAX_PROCS
proc_blk3:      .res MAX_PROCS
proc_blk5:      .res MAX_PROCS

;;; Drivers
drvs:           .res MAX_DRVS + 1
drv_pid:        .res MAX_DRVS + 1
drv_dev:        .res MAX_DRVS + 1
drv_vl:         .res MAX_DRVS + 1
drv_vh:         .res MAX_DRVS + 1
free_drv:       .res 1

;;; Drivers assigned to devices.
dev_drv:        .res MAX_DEVS

;;; KERNAL
old_kernal_vectors:
                .res 32

;;;;;;;;;;;;;
;;; LOCAL ;;;
;;;;;;;;;;;;;
;
; Per process.

    .segment "LOCALBSS"

.export tunix_blk1
.export tunix_blk2, lbanks, lbanksb
.export first_lbank, lfns, lfnsb
.export lfn_glfn, first_lfn, waiting
.export waitingb, waiting_pid, free_wait
.export first_waiting, pid, reg_a, reg_x
.export reg_y, stack, flags, saved_vic
.export multitasking, old_load, old_save

;; Vitals
tunix_blk1:     .res 1  ; Same for all.
tunix_blk2:     .res 1  ; Same for all.
old_load:       .res 2  ; KERNAL LOAD
old_save:       .res 2  ; KERNAL SAVE
pid:            .res 1
multitasking:   .res 1

;; Machine state
reg_a:          .res 1
reg_x:          .res 1
reg_y:          .res 1
flags:          .res 1
stack:          .res 1
saved_vic:      .res 16

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Additional local bank ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    .segment "LOCALBSS2"

;; Extended memory banks
; Deque of used ones.
lbanks:         .res MAX_BANKS
lbanksb:        .res MAX_BANKS
first_lbank:    .res 1

;; Process info
waiting:        .res MAX_PROCS
waitingb:       .res MAX_PROCS
waiting_pid:    .res MAX_PROCS
free_wait:      .res 1
first_waiting:  .res 1

;; Logical file numbers
; Deque of used ones
lfns:           .res MAX_LFNS
lfnsb:          .res MAX_LFNS
first_lfn:      .res 1
lfn_glfn:       .res MAX_LFNS

.ifdef BLEEDING_EDGE

.export signals, signalsb, signal_type
.export signal_payload
.export signal_handler_l
.export signal_handler_h
.export pending_signal_types
.export free_signal, pending_signal

;; Signals
signals:                .res MAX_SIGNALS
signalsb:               .res MAX_SIGNALS
signal_type:            .res MAX_SIGNALS
signal_payload:         .res MAX_SIGNALS
signal_handler_l:       .res MAX_SIGNALS
signal_handler_h:       .res MAX_SIGNALS
pending_signal_types:   .res 256
free_signal:            .res 1
pending_signal:         .res 1

.endif ; .if BLEEDING_EDGE

    .code

;;;;;;;;;;;;;;
;;; MACROS ;;;
;;;;;;;;;;;;;;
;
; Convenience macros to dampen down
; repetition.

.macro mvb to, from
    lda from
    sta to
.endmacro

.macro mvw to, from
    mvb to, from
    mvb to+1, from+1
.endmacro

.macro phx
    txa
    pha
.endmacro

.macro phy
    tya
    pha
.endmacro

.macro plx
    pla
    tax
.endmacro

.macro ply
    pla
    tay
.endmacro

.macro push from
    lda from
    pha
.endmacro

.macro pop to
    pla
    sta to
.endmacro

.macro pushw from
    push from
    push from+1
.endmacro

.macro popw to
    pop to+1
    pop to
.endmacro

.macro store_regs
    sta reg_a
    stx reg_x
    sty reg_y
.endmacro

.macro store_regs_and_flags
    store_regs
    php
    pla
    sta flags
.endmacro

.macro load_regs
    lda reg_a
    ldx reg_x
    ldy reg_y
.endmacro

.macro ldax from
    lda from
    ldx from+1
.endmacro

.macro ldaxi val
    lda #<(val)
    ldx #>(val)
.endmacro

.macro ldayi val
    lda #<(val)
    ldy #>(val)
.endmacro

.macro stax to
    sta to
    stx to+1
.endmacro

.macro stay to
    sta to
    sty to+1
.endmacro

.macro stwi to, val
    mvb to, #<(val)
    mvb to+1, #>(val)
.endmacro

.macro incw at
    inc at
    bne :+
    inc at+1
:
.endmacro

.macro qdecw at
    dec at
    bne :+
    dec at+1
:
.endmacro

.macro jqdecw at, to
    dec at
    bne to
    dec at+1
    bne to
.endmacro

.macro jmpa to, val
    lda val
    jmp to
.endmacro

.macro jsra to, val
    lda val
    jsr to
.endmacro

.macro jsrx to, val
    ldx val
    jsr to
.endmacro

.macro jsry to, val
    ldy val
    jsr to
.endmacro

;;;;;;;;;;;;;;;;;;;
;;; LIST MACROS ;;;
;;;;;;;;;;;;;;;;;;;
;
; Macros for singly-linked lists.
; 'lpop*' and 'lloop_*' also work with
; deques.
;
; There may be either multiple lists or
; deques in one set of arrays.

;; Pop from front of list.
; Ensure that lists aren't empty before
; use.

.macro lpop_x list, free
    ldx free
    mvb free, {list,x}
.endmacro

.macro lpop_y list, free
    ldy free
    mvb free, {list,y}
.endmacro

;; Push to front of list.

.macro lpush_x list, first
    mvb {list,x}, first
    stx first
.endmacro

.macro lpush_y list, first
    mvb {list,y}, first
    sty first
.endmacro

;; Move between lists.

.macro lmove_x list, from, to
    lpop_x list, from
    lpush_x list, to
.endmacro

.macro lmove_y list, from, to
    lpop_y list, from
    lpush_y list, to
.endmacro

;; Forwards iteration.

.macro lloop_x list, loop
    lda list,x
    tax
    bne loop
.endmacro

.macro lloop_y list, loop
    lda list,y
    tay
    bne loop
.endmacro

;;;;;;;;;;;;;;;;;;;;
;;; DEQUE MACROS ;;;
;;;;;;;;;;;;;;;;;;;;
;
; Deques are doubly-linked lists that
; allow fast random insert and remove
; operations.  An additional array con-
; tains the backwards pointers. (e.g.
; 'procsb' holds those for 'procs'.

;; Push to front of deque.

.macro dpush_x fw, bw, first
    mvb {bw,x}, #0
    phy
    ldy first
    beq :+
    txa
    sta bw,y
:   tya
    sta fw,x
    stx first
    ply
.endmacro

.macro dpush_y fw, bw, first
    mvb {bw,y}, #0
    phx
    ldx first
    beq :+
    tya
    sta bw,x
:   txa
    sta fw,y
    sty first
    plx
.endmacro

;; Remove from deque.

.macro drm_x fw, bw, first
    phy
    cpx first
    bne :+
    mvb first, {fw,x}
    jmp :++
    ; Link previous
:   ldy bw,x
    beq :+
    mvb {fw,y}, {fw,x}
    ; Link next
:   ldy fw,x
    beq :+
    mvb {bw,y}, {bw,x}
    ; Optional.
:   lda #0
    sta fw,x
    sta bw,x
    ply
.endmacro

.macro drm_y fw, bw, first
    phx
    cpy first
    bne :+
    mvb first, {fw,y}
    jmp :++
    ; Link previous
:   ldx bw,y
    beq :+
    mvb {fw,x}, {fw,y}
    ; Link next
:   ldx fw,y
    beq :+
    mvb {bw,x}, {bw,y}
    ; Optional.
:   lda #0
    sta fw,y
    sta bw,y
    plx
.endmacro

;; Allocate item in deque.
;
; Pops item from front of a 'free' list
; and pushes it onto the front of an
; 'allocated' list in the same deque
; array.

.macro dalloc_x fw, bw, from, to
    lpop_x fw, from
    dpush_x fw, bw, to
.endmacro

.macro dalloc_y fw, bw, from, to
    lpop_y fw, from
    dpush_y fw, bw, to
.endmacro

;; Move between deques
;
; Also used to move items back to free
; lists.

.macro dmove_x fw, bw, from, to
    drm_x fw, bw, from
    dpush_x fw, bw, to
.endmacro

.macro dmove_y fw, bw, from, to
    drm_y fw, bw, from
    dpush_y fw, bw, to
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BANK ALLOCATION MACROS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.macro alloc_bank_x
    lpop_x banks, free_bank
.endmacro

.macro alloc_lbank_x
    dpush_x lbanks, lbanksb, first_lbank
.endmacro

.macro free_bank_x
    lpush_x banks, free_bank
.endmacro

.macro free_lbank_x
    drm_x lbanks, lbanksb, first_lbank
.endmacro

.macro free_lbank_y
    drm_y lbanks, lbanksb, first_lbank
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCESS LIST MACROS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Allocate free process.

.macro alloc_proc_running_y
    dalloc_y procs, procsb, free_proc, running
.endmacro

;; Move between running to sleeping.

.macro mv_running_sleeping_x
    dmove_x procs, procsb, running, sleeping
.endmacro

.macro mv_sleeping_running_x
    dmove_x procs, procsb, sleeping, running
.endmacro

; Make zombie a free process.
.macro reap_zombie_x
    dmove_x procs, procsb, zombie, free_proc
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WAITING LIST MACROS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Allocate local slot in waiting list.
.macro alloc_waiting_y
    dalloc_y waiting, waitingb, free_wait, first_waiting
.endmacro

; Remove slot from waiting list.
.macro free_waiting_y
    dmove_y waiting, waitingb, first_waiting,free_wait
.endmacro

;;;;;;;;;;;;;;;;;;;;;
;;; SIGNAL MACROS ;;;
;;;;;;;;;;;;;;;;;;;;;

.macro alloc_signal_y
    dalloc_y signals, signalsb, free_signal, pending_signal
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IO PAGE LIST MACROS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

.macro alloc_iopage_x
    dalloc_x iopages, iopagesb, free_iopage, first_iopage
.endmacro

.macro free_iopage_x
    dmove_x iopages, iopagesb, first_iopage, free_iopage
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCESS BANK MACROS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCESS DATA BANK (on RAM123) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.macro enter_procdata_x
    push ram123
    mvb ram123, {proc_data,x}
.endmacro

.macro enter_procdata_y
    push ram123
    mvb ram123, {proc_data,y}
.endmacro

.macro leave_procdata
    pop ram123
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONTEXT (PROCESS DATA & IO) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.macro enter_context_x
    push io23
    mvb io23, {proc_io23,x}
    enter_procdata_x
.endmacro

.macro leave_context
    leave_procdata
    pop io23
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCESS BANKS (non-kernel) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.macro enter_banks_x
    push ram123
    push blk1
    push blk2
    mvb ram123, {proc_ram123,x}
    lda proc_blk2,x
    pha
    mvb blk1, {proc_blk1,x}
    pop blk2
.endmacro

.macro leave_banks
    pop blk2
    pop blk1
    pop ram123
.endmacro

    .segment "LIB"

;;;;;;;;;;;;;;;;;;;;;
;;; ZERO PAGE LIB ;;;
;;;;;;;;;;;;;;;;;;;;;

; Init s, d and c with values at XA.
.export sset
.proc sset
    sta p+1
    stx p+2
    ldx #5
p:  mvb {s,x}, {$ff00,x}
    dex
    bpl p
    rts
.endproc

; Init s, d and c with values at XA.
.export dset
.proc dset
    sta p+1
    stx p+2
    ldx #3
p:  mvb {d,x}, {$ff00,x}
    dex
    bpl p
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MEMORY BLOCK CLEAR/MOVE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TODO: Enhance src/lib/blit and use
; that instead.

; Copy descriptor 'set' over copy
; vectors and move it, baby.
.macro smemcpyax set
    ldaxi set
    jsr smemcpy
.endmacro

; Copy range at XA.
.export smemcpy
.proc smemcpy
    stax tmp3
    pushw s
    pushw d
    pushw c
    ldax tmp3
    jsr sset
    jsr memcpy
    popw c
    popw d
    popw s
    rts
.endproc

; Copy memory (forwards).
; s: source
; d: destination
; c: count
.export memcpy
.proc memcpy
    phy
    ldy #0
    ldx cl
    inx
    inc ch
    bne copy_forwards ; (jmp)

l:  mvb {(d),y}, {(s),y}
    iny
    beq k
copy_forwards:
q:  dex
    bne l
    dec ch
    bne l
r:  ply
    rts
k:  inc sh
    inc dh
    jmp q
.endproc

; Copy descriptor 'set' over copy
; vectors and move it, baby.
.macro sbzeroax set
    ldaxi set
    jsr sbzero
.endmacro

; Copy range at XA.
.export sbzero
.proc sbzero
    stax tmp3
    pushw s
    pushw d
    pushw c
    ldax tmp3
    jsr dset
    jsr bzero
    popw c
    popw d
    popw s
    rts
.endproc

; Clear memory.
; d, c: dest, counter
.export bzero
.proc bzero
    ldx cl
    inx
    inc ch
    ldy dl
    mvb dl, #0
    beq +n ; (jmp)
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

.export cmpmem
.proc cmpmem
    ldx cl
    inx
    inc ch
    ldy #0
    beq :+ ; (jmp)
l:  lda (s),y
    cmp (d),y
    bne r
    iny
    beq y0
:   dex
    bne l
    dec ch
    bne l
r:  rts
y0: inc sh
    inc dh
    bne :- ; (jmp)
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SOFT SEGMENTATION FAULT CHECKS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Make or validate 8-bit CRC checksum.
; s: Start of block
; c: Size of block
; A: : 0 to make CRC or CRC to validate.
; Returns:
;  A/Z = 0 if CRC has been validated.
.export crc8
.proc crc8
    sta tmp1+1
    ldx cl
    dex
    dec ch
    ldy #0
    beq t ; (jmp)
l:  lda (s),y
    sta tmp1
    ; Make CRC checksum
    lda tmp1+1
    eor tmp1
    rol tmp1
    eor tmp1
    rol tmp1
    eor tmp1
    rol tmp1
    eor tmp1
    rol tmp1
    eor tmp1
    rol tmp1
    eor tmp1
    rol tmp1
    eor tmp1
    rol tmp1
    eor tmp1
    sta tmp1+1
    iny
    bne t
    inc sh
t:  dex
    bne l
    dec ch
    bne l
    lda tmp1+1
    rts
.endproc

;;;;;;;;;;;;;;;;;;
;;; LIST UTILS ;;;
;;;;;;;;;;;;;;;;;;

.export list_length
.proc list_length
    stax tmp1
    pushw zp1
    mvw zp1,tmp1
    ldx #0
    cpy #0
    beq empty
:   inx
    lda (zp1),y
    tay
    bne :-
empty:
    popw zp1
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;
;;; USER INTERFACE ;;;
;;;;;;;;;;;;;;;;;;;;;;
;
; Blah.

.macro error asciiz
    stwi zp2, asciiz
    jmp error_
.endmacro

.proc error_
    sei
    jsr FRESTOR
    jsr printstr
:   jmp :-
.endproc

.export printstr
.proc printstr
    mvb tmp1, #0
    jsr CLRCHN
    phx
l:  ldy #0
    lda (zp2),y
    beq r
    jsr BSOUT
    incw zp2
    jmp l
r:  plx
    rts
.endproc

.macro print asciiz
    pushw zp2
    stwi zp2, asciiz
    jsr printstr
    popw zp2
.endmacro

.export print_cr
.proc print_cr
    phx
    jsra BSOUT, #13
    plx
    rts
.endproc

.export print_comma
.proc print_comma
    phx
    jsra BSOUT, #','
    plx
    rts
.endproc

.export outa
.proc outa
    ldy #0
    sta (d),y
    incw d
    rts
.endproc

.macro out val
    jsra outa, val
.endmacro

.macro outw at
    jsra outa, at
    jsra outa, at+1
.endmacro

    .segment "KERNEL"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SYSTEM CALL RETURN DATA ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.export print_decbyte
.proc print_decbyte
    sta tmp1
    phx
    ldx tmp1
    jsra PRTFIX, #0
    plx
    rts
.endproc

.export print_decword
.proc print_decword
    sta tmp1
    txa
    jmpa PRTFIX, tmp1
.endproc

.export print_decbyte_x8
.proc print_decbyte_x8
    ldx #0
    stx tmp1
    clc
    rol
    rol tmp1
    clc
    rol
    rol tmp1
    clc
    rol
    rol tmp1
    tax
    jmpa PRTFIX, tmp1
.endproc

; Print '$' and hexadecimal byte in A.
.export print_hexbyte
.proc print_hexbyte
    pha
    jsra BSOUT, #'$'
    pla
    pha
    lsr
    lsr
    lsr
    lsr
    jsr print_nibble
    pla
.endproc

; Print hexadecimal low nibble in A.
.export print_nibble
.proc print_nibble
    and #$0f
    cmp #10
    bcc :+
    adc #6
:   adc #48
    jmp BSOUT
.endproc

; Print comma followed by hexadecimal
; byte in A.
.export print_chb
.proc print_chb
    sta tmp1
    pushw zp2
    phx
    jsra BSOUT, #','
    jsra print_hexbyte, tmp1
    plx
    popw zp2
    rts
.endproc

; Print comma followed by deciaml byte
; in A.
.export print_cv
.proc print_cv
    sta tmp1
    pushw zp2
    phx
    jsra BSOUT, #','
    jsra print_decbyte, tmp1
    plx
    popw zp2
    rts
.endproc

; Print string at zp2 and step over the
; terminating 0.
.export print_head
.proc print_head
    phx
    jsr printstr
    incw zp2
    plx
    rts
.endproc

; Print list indexes as CSV line.
.macro print_csv list, first
    ldx first
    beq :++
    txa
:   jsr print_cv
    lda list,x
    tax
    bne :-
    jsr print_cr
:
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;
;;; EXTENDED MEMORY ;;;
;;;;;;;;;;;;;;;;;;;;;;;

; Allocate bank
; Returns:
;  X & A: Bank #, 0 if out of memory.
;  Z: 0: Out of memory.
.export balloc
.proc balloc
    phy
    alloc_bank_x
    cpx #0
    beq n  ; Oops…
    alloc_lbank_x
    inc bank_refs,x
n:  ply
    txa
    rts
.endproc

.export free_lbank
.proc free_lbank
    cpy #0
    beq r
    free_lbank_y
r:  rts
.endproc

.export free_lbank_a
.proc free_lbank_a
    sta tmp1
    phy
    jsry free_lbank, tmp1
    ply
    rts
.endproc

; Free bank
; X: Bank #
.export bfree
.proc bfree
    ; TODO: Check lbank ownership.
    dec bank_refs,x
    bmi invalid_bank
    bne :+
    free_bank_x
:   free_lbank_x
    clc
    rts
invalid_bank:
    error err_invalid_bank
    inc bank_refs,x
    sec
    rts
.endproc

    .segment "KERNELDATA"

err_invalid_bank:
    .byte "BFREE INVALID BANK.", 0

    .segment "KERNEL"

; Free all banks of current process.
.export free_lbanks
.proc free_lbanks
    ; TODO: Check lbank ownership.
    ldx first_lbank
    beq r
:   lda lbanks,x
    pha
    jsr bfree
    plx
    bne :-
r:  rts
.endproc

items_mem_info:
  .byte "FREE", 0
  .byte "USED", 0
  .byte "RESERVED", 0
  .byte "FAULTY", 0
  .byte "TOTAL", 0
  .byte "BANKSIZE,8192", 0
  .byte 0

.export mem_info
.proc mem_info
    pushw zp2
    stwi zp2, items_mem_info

    jsr print_head
    ldaxi banks
    jsry list_length, free_bank
    txa
    jsr print_cv
    jsr print_cr

    jsr print_head
    ldaxi banks
    jsry list_length, free_bank
    stx tmp1
    lda banks_ok
    sec
    sbc tmp1
    jsr print_cv
    jsr print_cr

    jsr print_head
    ldax #FIRST_BANK
    jsr print_cv
    jsr print_cr

    jsr print_head
    ldax banks_faulty
    jsr print_cv
    jsr print_cr

    ; Total
    jsr print_head
    lda banks_ok
    clc
    adc #FIRST_BANK
    ldx #0
    jsr print_cv
    jsr print_cr

    ; Bank size (in head)
    jsr print_head
    jsr print_cr

    popw zp2
    clc
    rts
no_proc:
    sec
    rts
.endproc

;;;;;;;;;;;;;;;;;;
;;; SPEED COPY ;;;
;;;;;;;;;;;;;;;;;;
;
; Generates memory moving speed code
; across multiple extended memory banks.
;
; ATM a block copy from BLK3 to BLK5 is
; created, starting with bank
; 'copy_bank'.

    .segment "KERNELDATA"

txt_speed_code:
  .byte "MAKING SPEED CODE.", 0
txt_carriage_return:
  .byte 13, 0

    .segment "KERNEL"

; Make all wanted block moves.
.export gen_speedcodes
.proc gen_speedcodes
    print txt_speed_code
    push blk5
    pushw zp1
    pushw c

    ;; Make copy from BLK3 to BLK5.
    jsr balloc
    sta speedcopy_blk3_to_blk5
    sta blk5
    ; Source/dest argument values.
    stwi zp1, $6000
    stwi ptr1, $a000
    ; Total count (quick countdown).
    stwi c, $2000
    jsr gen_speedcode
    out #OP_RTS

    ;; Make copy from lowmem to BLK5.
    jsr balloc
    sta speedcopy_lowmem_to_blk5
    sta blk5
    stwi zp1, $0000
    stwi ptr1, $b000
    stwi c, $0400
    jsr gen_speedcode
    out #OP_RTS

    ;; Make copy from BLK5 to lowmem.
    jsr balloc
    sta speedcopy_blk5_to_lowmem
    sta blk5
    stwi zp1, $b000
    stwi ptr1, $0000
    stwi c, $0400
    jsr gen_speedcode
    out #OP_JMP_ABS

    popw c
    popw zp1
    pop blk5
    print txt_carriage_return
    rts
.endproc

; Make single block copy.
.export gen_speedcode
.proc gen_speedcode
next_bank:
    ; Per bank move count.
    stwi tmp1, ($2000 / 6)
    ; Bank fill pointer.
    stwi d, $a000

next_move:
    ;; Make move.
    out #OP_LDA_ABS
    outw zp1
    out #OP_STA_ABS
    outw ptr1

    ;; Step
    ; Increment argument values.
    incw zp1
    incw ptr1
    ; Decrement total move count.
    qdecw c
    beq done
    ; Decrement per bank move count.
    jqdecw tmp1, next_move

    ;; Make switch to next bank.
    out #OP_LDA_IMM
    jsr balloc
    pha
    jsr outa
    out #OP_JMP_ABS
    out #<next_speedcopy
    out #>next_speedcopy
    pop blk5
    jmp next_bank

done:
    rts
.endproc

.export next_speedcopy
.proc next_speedcopy
    sta blk2
    jmp $4000
.endproc

;;;;;;;;;;;;;;;;;;;;;;
;;; MACHDEP VIC-20 :::
;;;;;;;;;;;;;;;;;;;;;;
;
; Process forks and switches.  TUNIX is
; not ready to support other platforms.

    .segment "MACHDEPDATA"

vec_vic_to_blk5:
  .word $9000, saved_vic+$2000, $0010
vec_screen_to_blk5:
  .word $1000, $a000, $1000
vec_color_to_blk5:
  .word $9400, $b400, $0400

vec_blk5_to_screen:
  .word $a000, $1000, $1000
vec_blk5_to_color:
  .word $b400, $9400, $0400
vec_blk5_to_vic:
  .word saved_vic+$2000, $9000, $0010

vec_io23_to_blk5:
  .word $9800, $b800, $07f0

.macro save_internal_ram_to_blk5
    push blk2
    jsra next_speedcopy, speedcopy_lowmem_to_blk5
    pop blk2
    smemcpyax vec_screen_to_blk5
    smemcpyax vec_color_to_blk5
    smemcpyax vec_vic_to_blk5
.endmacro

    .segment "MACHDEPDATA"

; Fork banks and save stack.
;
; The child will never see this function
; but return from it.
;
; Y: child process ID
; Returns Y unaffected.
.export machdep_fork
.proc machdep_fork
    ldx pid
    stx tmp1
    mvb tmp2, {proc_data,x}
    ; Save sp for child to return with.
    tsx
    stx tmp2+1

    push ram123
    push io23
    push blk2
    push blk3
    push blk5

    ;;; Clone child's per-process banks.
    ;; Clone IO23.
    alloc_bank_x
    inc bank_refs,x
    txa
    sta proc_io23,y
    pha
    ; Copy parent's IO23 into child's.
    sta blk5
    smemcpyax vec_io23_to_blk5
    ; Copy lowmem, screen, color & VIC.
    sei
    save_internal_ram_to_blk5
    cli

    ;; Clone procdata RAM123.
    alloc_bank_x
    inc bank_refs,x
    txa
    sta ram123 ; Bank in procdata.
    sta proc_data,y
    alloc_lbank_x
    ; Copy parent's into child's.
    mvb blk3, tmp2 ; (Parent's procdata)
    stx blk5
    jsra next_speedcopy, speedcopy_blk3_to_blk5
    mvb blk2, tunix_blk2
    ; Finish up IO23.
    pop io23 ; Bank in IO23.
    tax
    alloc_lbank_x
    ; Set child ID and stack pointer.
    sty pid
    mvb stack, tmp2+1

    ;; Clone remaining banks.
    .macro alloc_blk_y procblk
        jsr balloc
        sta procblk,y
    .endmacro
    alloc_blk_y proc_ram123
    alloc_blk_y proc_blk1
    alloc_blk_y proc_blk2
    alloc_blk_y proc_blk3
    alloc_blk_y proc_blk5

    ;; Ensure banks have been allocated.
    lda proc_data,y
    beq o
    lda proc_ram123,y
    beq o
    lda proc_io23,y
    beq o
    lda proc_blk1,y
    beq o
    lda proc_blk2,y
    beq o
    lda proc_blk3,y
    beq o
    lda proc_blk5,y
    bne banks_complete
o:  jmp out_of_memory
banks_complete:

    ; Copy old into new banks.
    .macro copy_blk_y procblk
        ldx tmp1 ; parent
        mvb blk3, {procblk,x}
        mvb blk5, {procblk,y}
        jsra next_speedcopy, speedcopy_blk3_to_blk5
        mvb blk2, tunix_blk2
    .endmacro
    copy_blk_y proc_ram123
    copy_blk_y proc_blk1
    copy_blk_y proc_blk2
    copy_blk_y proc_blk3
    copy_blk_y proc_blk5

    clc

r:  pop blk5
    pop blk3
    pop blk2
    pop io23
    pop ram123
    rts

.export out_of_memory
out_of_memory:
    jsr free_lbanks
    sec
    bcs r ; (jmp)
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOGICAL FILE NUMBERS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    .segment "KERNEL"

; Translate local to global LFN.
; Creates missing ones.
; X: LFN
.export lfn_to_glfn
.proc lfn_to_glfn
    tax
    lda lfn_glfn,x
    bne :+  ; Use existing...
    ; Get LFN slot.
    lpush_x lfns, first_lfn
    ; Allocate GLFN.
    phx
    lpop_x glfns, free_glfn
    inc glfn_refs,x
    ; Link GLFN to LFN.
    stx tmp1
    plx
    mvb {lfn_glfn,x}, tmp1
:   rts
.endproc

;; Free all LFNs of the current process.
.export free_lfns
.proc free_lfns
    ldy first_lfn
    beq r
:   ldx lfn_glfn,y
    dec glfn_refs,x
    bne :+  ; (Still used.)
    lpush_x glfns, free_glfn
:   lloop_y lfns, :--
r:  rts
.endproc

;;;;;;;;;;;;;;;;;
;;; PROCESSES ;;;
;;;;;;;;;;;;;;;;;
;
; Low 1K, screen, color and VIC are
; on the same bank as the IO23 area,
; which is reserved for TUNIX and
; drivers.  A procdata RAM123, only
; banked in for TUNIX, holds additional
; per-process data.

.export unref_parent_banks
.proc unref_parent_banks
    ;; Remove banks of parent from
    ;; child's lbank list.
    .macro unref_lbank procblk
        jsra free_lbank_a, {procblk,x}
    .endmacro
    unref_lbank proc_data
    unref_lbank proc_ram123
    unref_lbank proc_io23
    unref_lbank proc_blk1
    unref_lbank proc_blk2
    unref_lbank proc_blk3
    unref_lbank proc_blk5
    rts
.endproc

.export increment_glfn_refs
.proc increment_glfn_refs
    ldx first_lfn
    beq :++
:   inc glfn_refs,x
    lloop_x lfns, :-
:   rts
.endproc

.export update_forked_data
.proc update_forked_data
    enter_procdata_y
    jsr unref_parent_banks
    jsr increment_glfn_refs
    leave_procdata
    rts
.endproc

; Fork current process.
; Returns:
; A: New process ID for the parent and
;    0 for the child.
.export fork
.proc fork
    alloc_proc_running_y
    cpy #0
    beq no_more_procs
    mvb {proc_flags,y}, #PROC_BABY

    ldx pid
    phx
    jsr machdep_fork
    plx     ; Parent's ID.
    bcs e

    cpx pid ; Real ID.
    bne child
    jsr update_forked_data

r:  tya
    clc
    rts

e:  cpx pid ; Real ID.
    bne child
    dmove_y procs, procsb, running, free_proc
    mvb {proc_flags,y}, 0

no_more_procs:
    lda #255
    sec
    rts

child:
    ; Map in rest of child's banks for
    ; the first time.  RAM123, BLK1 and
    ; BLK2 will be mapped in by
    ; tunix_leave().
    mvb ram123, {proc_data,y}
    mvb io23, {proc_io23,y}
    mvb blk3, {proc_blk3,y}
    mvb blk5, {proc_blk5,y}

    lda #0
    clc
    rts
.endproc

; Suspend process.
; X: Process ID
.export suspend
.proc suspend
    lda proc_flags,x
    beq not_there
    bmi already_sleeping
    mv_running_sleeping_x
    mvb {proc_flags,x}, #PROC_SLEEPING
    clc
    rts
not_there:
already_sleeping:
    sec
    rts
.endproc

; Wake process up.
; X: Process ID.
.export resume
.proc resume
    lda proc_flags,x
    bpl not_to_resume
    mv_sleeping_running_x
    mvb {proc_flags,x}, #PROC_RUNNING
    clc
    rts
not_to_resume:
    sec
    rts
.endproc

.proc proc_free_resources
    phx
    enter_context_x
    jsr free_lfns
    jsr free_lbanks
    leave_context
    jsr free_iopages
    jsr free_drivers
    plx
    rts
.endproc

; Free all resources of a process and
; turn it into a zombie.
; X: Process ID.
.export zombify
.proc zombify
    jsr proc_free_resources

    ;; Remove process from running or
    ;; sleeping list.
    lda proc_flags,x
    bmi :+
    drm_x procs, procsb, running
    jmp put_on_zombie_list
:   drm_x procs, procsb, sleeping

put_on_zombie_list:
    dpush_x procs, procsb, zombie
    mvb {proc_flags,x}, #PROC_ZOMBIE
    rts
.endproc

; Take process off waiting list.
; X: Process waiting for.
; Y: Slot in waiting list.
; Returns:
;  A & Y: Slot of next in waiting list
;         or 0 (with zero flag true.)
.export free_waiting
.proc free_waiting
    enter_procdata_x
    free_waiting_y
    ldy first_waiting
    leave_procdata
    tya ; (cpy #0)
    rts
.endproc

; X: Process waiting for.
; Returns:
;  A & Y: Slot in waiting list.
.export get_waiting
.proc get_waiting
    enter_procdata_x
    ldy first_waiting
    ldx waiting_pid,y
    leave_procdata
    tya ; (cpy #0)
    rts
.endproc

; X: Process waiting for.
; Returns:
;  Y: Slot in waiting list.
.export alloc_waiting
.proc alloc_waiting
    enter_procdata_x
    alloc_waiting_y
    mvb {waiting_pid,y}, pid
    leave_procdata
    ; TODO: Check if out of memory.
    rts
.endproc

; Wait for process to exit
; X: Process ID
; Returns:
; A: Exit code
.export wait
.proc wait
    lda proc_flags,x
    beq invalid_pid
    jsr alloc_waiting

check_if_zombie:
    lda proc_flags,x
    cmp #PROC_ZOMBIE
    beq end_wait
    phx
    phy
    jsrx suspend, pid
    jsr schedule
    ply
    plx
    jmp check_if_zombie

invalid_pid:
    sec
    rts

end_wait:
    jsr free_waiting
    beq reap_zombie
    phx
    jsr resume_waiting
    plx

return_code:
    lda exit_codes,x
    clc
    rts

reap_zombie:
    reap_zombie_x
    lda #0
    sta proc_flags,x
    sta proc_data,x
    sta proc_io23,x
    sta proc_ram123,x
    sta proc_blk1,x
    sta proc_blk2,x
    sta proc_blk3,x
    sta proc_blk5,x
    beq return_code ; (jmp)
.endproc

; Exit current process
; A: Exit code
.export exit
.proc exit
    mvb multitasking, #1
    jsrx zombify, pid
.endproc

; Resume waiting process
; X: ID of process waiting for
.export resume_waiting
.proc resume_waiting
    mvb multitasking, #1
    jsr get_waiting
    beq r
    jmp resume
r:  rts
.endproc

; Kill process with exit code in A.
; X: Process ID
; A: Exit code
.export kill
.proc kill
    sta exit_codes,x
    lda proc_flags,x
    beq invalid_pid
    jsr zombify
    clc
    rts
invalid_pid:
    sec
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;
;;; PROCESS INFO ;;;
;;;;;;;;;;;;;;;;;;;;

    .segment "KERNEL"

.export proc_list_item
.proc proc_list_item
    phx
    txa
    jsr print_decbyte
    plx

    jsr print_comma
    phx
    ldy #'?'
    lda proc_flags,x
    cmp #PROC_RUNNING
    bne :+
    ldy #'R'
    bne prt ; (jmp)
:   cmp #PROC_SLEEPING
    bne :+
    ldy #'S'
    bne prt ; (jmp)
:   cmp #PROC_BABY
    bne :+
    ldy #'B'
    bne prt ; (jmp)
:   cmp #PROC_ZOMBIE
    bne prt
    ldy #'Z'
prt:tya
    jsr BSOUT
    plx

    phx
    jsr print_comma
    enter_procdata_x
    ldaxi lbanks
    jsry list_length, first_lbank
    leave_procdata
    txa
    jsr print_decbyte
    plx

    phx
    jsr print_comma
    enter_procdata_x
    ldaxi lfns
    jsry list_length, first_lfn
    leave_procdata
    txa
    jsr print_decbyte
    plx

    phx
    jsr print_comma
    enter_procdata_x
    ldaxi waiting
    jsry list_length, first_waiting
    leave_procdata
    txa
    jsr print_decbyte
    plx

    phx
    jsr print_comma
    jsra print_decbyte, {exit_codes,x}
    plx

    jmp print_cr
.endproc

    .segment "KERNELDATA"

items_proc_list:
  .byte "ID", 0
  .byte "FLAGS", 0
  .byte "#BANK", 0
  .byte "#LFN", 0
  .byte "#WAITING", 0
  .byte "EXITCODE", 0
  .byte 0

    .segment "KERNEL"

; Print process list
; X: process ID
.export proc_list
.proc proc_list
    pushw zp2
    stwi zp2, items_proc_list
    jsr print_head
    jsr print_comma
    jsr print_head
    jsr print_comma
    jsr print_head
    jsr print_comma
    jsr print_head
    jsr print_comma
    jsr print_head
    jsr print_comma
    jsr print_head
    jsr print_cr

    ldx running
    beq :+
l1: jsr proc_list_item
    lloop_x procs, l1
:

    ldx sleeping
    beq :+
l2: jsr proc_list_item
    lloop_x procs, l2
:

    ldx zombie
    beq :+
l3: jsr proc_list_item
    lloop_x procs, l3
:

    jsrx proc_list_item, #0

    popw zp2
    clc
    rts
.endproc

    .segment "KERNELDATA"

items_proc_info:
  .byte "ID", 0
  .byte "MEMORY", 0
  .byte "LFNS", 0
  .byte "#BANKS", 0
  .byte 0

; Print process info
; X: process ID
.export proc_info
.proc proc_info
    lda proc_flags,x
    bne :+
    jmp no_proc

:   stx tmp2
    pushw zp2
    stwi zp2, items_proc_info

    jsr print_head
    lda tmp2
    tax
    jsr print_cv
    jsr print_cr

    jsr print_head
    jsra print_chb, {proc_data,x}
    jsra print_chb, {proc_io23,x}
    jsra print_chb, {proc_ram123,x}
    jsra print_chb, {proc_blk1,x}
    jsra print_chb, {proc_blk2,x}
    jsra print_chb, {proc_blk3,x}
    jsra print_chb, {proc_blk5,x}
    jsr print_cr

    jsr print_head
    print_csv lfns, first_lfn

    jsr print_head
    ldaxi lbanks
    jsry list_length, first_lbank
    txa
    jsr print_cv
    jsr print_cr

    popw zp2
    clc
    rts
no_proc:
    sec
    rts
.endproc

;;;;;;;;;;;;;;;
;;; SIGNALS ;;;
;;;;;;;;;;;;;;;

.ifdef BLEEDING_EDGE

; Send signal
; X: process ID
; Y: signal type (0-255)
; A: payload
.export signal
.proc signal
    sty tmp1    ; type
    sta tmp2    ; payload
    lda proc_flags,x
    beq invalid_pid
    enter_procdata_x

    ; Do nothing if signal of the same
    ; type is already pending.
    lda pending_signal_types,y
    bne ignore

retry:
    ; Grab a slot on the queue.
    lda free_signal
    beq out_of_slots
    alloc_signal_y
    tya
    ; Save on local type scoreboard.
    sta pending_signal_types,y
    mvb {signal_type,y}, tmp1
    mvb {signal_payload,y}, tmp2
    jmp resume

ignore:
    leave_procdata
    clc
    rts

out_of_slots:
    phx
    jsr resume
    plx
    jsr schedule
    jmp retry

invalid_pid:
    sec
    rts
.endproc

; Deliver pending signal to current
; process.  For use in tunix_leave().
.macro sigaction1
    ; Do not deliver if one is still
    ; processed.
    lda is_processing_signal
    bne :++
    ; Grap pending.
:   dpopx pending_signals
    cpx #0
    beq :+ ; No signal pending.
    lda signal_type,x
    tay
    inc is_processing_signal
    mvb {pending_signal_types,y}, #0
    mvb sigjmp+1, {signal_handler_l,x}
    mvb sigjmp+2, {signal_handler_h,x}
:
.endmacro

.macro sigaction2
    php
    pha
    lda sigjmp+2
    beq :+
    phx
    phy
sigjmp:
    jsr $1234
    mvb sigjmp+2, #0
    ply
    plx
    dec is_processing_signal
:   pla
    plp
.endmacro

; Register handler for signal type.
; X: type
; A: handler high byte
; Y: handler low byte
.export set_handler
.proc set_handler
    sta tmp1
    sta signal_handler_h,x
    tya
    sta signal_handler_l,x
    clc
    rts
.endproc

; Unregister handler for signal type.
; X: type
; Returns with error if no handler was
; registered for the type.
.export reset_handler
.proc reset_handler
    lda signal_handler_h,x
    bne no_handler_set
    mvb {signal_handler_h,x}, #0
    clc
    rts
no_handler_set:
    sec
    rts
.endproc

.endif ; .if BLEEDING_EDGE

;;;;;;;;;;;;;;;;;
;;; I/O PAGES ;;;
;;;;;;;;;;;;;;;;;

; Free IO pages of process.
.export free_iopages
.proc free_iopages
    ldx pid
    ldy first_iopage
    beq r
l:  lda iopage_pid,y
    cmp pid
    bne n
    tax
    free_iopage_x
n:  lloop_y iopages, l
r:  rts
.endproc

; "DA"
.export tunix_iopage_alloc
.proc tunix_iopage_alloc
    lda free_iopage
    beq no_more
    alloc_iopage_x
    mvb {iopage_pid,x}, pid
    txa
    clc
    adc #IOPAGE_BASE - 1
    jmp respond
no_more:
    jmp respond_error
.endproc

; "DCp"
.export tunix_iopage_commit
.proc tunix_iopage_commit
    ;; Loop through all other processes.
    ldy #0
l:  cpy pid
    beq next
    lda proc_flags,y
    beq next

    ;; Copy page.
    phy
    push blk5
    mvb blk5, {proc_io23,x}
    lda SA
    clc
    adc #IOPAGE_BASE - 1
    sta sh
    clc
    adc #$20    ; (Bump to BLK5.)
    sta dh
    lda #0
    sta sl
    sta dl
    tay
:   lda (s),y
    sta (d),y
    iny
    bne :-
    pop blk5
    ply
next:
    iny
    cpy #MAX_PROCS
    bne l
    jmp respond_ok
.endproc

; "DFp"
.export tunix_iopage_free
.proc tunix_iopage_free
    lda SA
    sec
    sbc #IOPAGE_BASE - 1
    tax
    lda iopage_pid,x
    bmi not_there   ; Page is free...
    cmp pid
    bne not_there   ; Not ours...
    mvb {iopage_pid,x}, #255
    free_iopage_x
    jmp respond_ok
not_there:
    jmp respond_error
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DRIVER REGISTRATION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Free drivers of process.
free_drivers: ; TODO: Deque instead of list.
rts
    ldx pid
    ;ldy first_drv
    beq r
l:  lda drv_pid,y
    cmp pid
    bne n
    tax
    lpush_x drvs, free_drv
    ; Set device to KERNAL.
    lda drv_dev,y
    tax
    mvb {dev_drv,x}, #0 ; KERNAL
n:  lloop_y drvs, l
r:  rts

; Register driver and assign to device
; XA: vectors
; Y:  device
; Returns:
; X:  driver ID or 0.
.export register
.proc register
    stax zp1

    ;; Get slot.
    lpop_x drvs, free_drv
    beq r

    ;; Populate slot.
    mvb {drv_pid,x}, pid
    tya
    sta drv_dev,x
    mvb {drv_vl,x}, zp1
    mvb {drv_vh,x}, zp1+1

    ;; Assign to device.
    txa
    sta dev_drv,y

r:  rts
.endproc

; "DR"
.export tunix_register
.proc tunix_register
    ldy SA
    ldax filename+2
    jsr register
    bcs r
    txa
    jmp respond
r:  jmp respond_error
.endproc


;;;;;;;;;;;;;;;;;
;;; PROCESS 0 ;;;
;;;;;;;;;;;;;;;;;

    .segment "KERNELDATA"

msg_exiting_init:
  .byte "NOTHING TO RUN.", 13, 0
  .byte "EXITING INIT.", 13, 0

msg_shutdown:
  .byte "SYSTEM DOWN. BYE!", 13, 13, 0

    .segment "KERNEL"

.export proc0
.proc proc0
    jsrx suspend, #0
    jsr schedule
    lda running
    bne proc0
    mvb multitasking, #0
    print msg_exiting_init
    jsr proc_list
    print msg_shutdown
    jsr basic_cold_init
    jmp get_ready
.endproc

.export halt
.proc halt
    jmp halt
.endproc

.export guru_meditation
.proc guru_meditation
    jmp guru_meditation
.endproc

.export stack_overflow
.proc stack_overflow
    jmp stack_overflow
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SYSCALL DRIVER (DEVICE #31) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Handles system calls via device
; TUNIX_DEVICE (#31 by default).

.export filename
.export response, response_len
.export responsep

    .segment "LOCALBSS"

filename:       .res 256

    .segment "LOCALBSS2"

;; Syscalls
response:       .res 8
response_len:   .res 1
responsep:      .res 1

    .segment "KERNELDATA"

.export tunix_driver
tunix_driver:
  .word tunix_open, tunix, tunix
  .word tunix, tunix, tunix_basin
  .word tunix, tunix, tunix, tunix
  .word tunix, tunix, tunix, tunix
  .word tunix

    .segment "KERNEL"

;; System call without arguments that
;; might fail.
.macro syscall0 name, fun
    .export name
    .proc name
        jsr fun
        bcs :+
        jmp respond_ok
:       jmp respond_error
    .endproc
.endmacro

;; System call with byte argument that
;; will never fail.
.macro syscall1v name, fun, load
    .export name
    .proc name
        load SA
        jmp fun
    .endproc
.endmacro

;; System call with byte argument that
;; might fail.
.macro syscall1 name, fun, load
    .export name
    .proc name
        load SA
        jsr fun
        bcs :+
        jmp respond_ok
:       jmp respond_error
    .endproc
.endmacro

;; Do nothing.
.export tunix
.proc tunix
    sec
    rts
.endproc

.export tunix_open
.proc tunix_open
    lda FNLEN
    beq s
    lda filename
    cmp #'M'
    beq tunix_memory
    cmp #'P'
    beq p
    cmp #'G'
    beq g
    cmp #'D'
    beq d
    jmp respond_error
p:  jmp tunix_procs
d:  jmp tunix_drivers
s:  jmp schedule
g:  jmp tunix_general
.endproc

.export respond_error
.proc respond_error
    ldx #0
    stx responsep
    inx
    stx response
    stx response_len
    sec
    rts
.endproc

.export respond_ok
.proc respond_ok
    ldx #0
    stx STATUS
    stx response
    stx responsep
    inx
.endproc

.export respond_len
.proc respond_len
    stx response_len
    clc
    rts
.endproc

.export respond1
.proc respond1
    sta response
    ldx #0
    stx STATUS
    inx
    bne respond_len ; (jmp)
.endproc
    
; Respond with value and error code 0.
; A: value
.export respond
.proc respond
    pha
    sta response+1
    lda #0
    sta response
    sta responsep
    ldx #2
    pla
    jmp respond_len
.endproc

.export tunix_basin
.proc tunix_basin
    ldx responsep
    cpx response_len
    beq :+
    lda response,x
    inc responsep
    clc
    rts
:   sec
    rts
.endproc

;; GENERAL

; "G?"
.export tunix_general
.proc tunix_general
    lda filename+1
    cmp #'M'
    beq tunix_mode
    jmp respond_error
.endproc

.export tunix_mode
.proc tunix_mode
    mvb multitasking, SA
    clc
    rts
.endproc

;; EXTENDED MEMORY

; "M?"
.export tunix_memory
.proc tunix_memory
    lda filename+1
    cmp #'A'
    beq tunix_balloc
    cmp #'F'
    beq tunix_bfree
    cmp #'I'
    beq tunix_mem_info
    jmp respond_error
.endproc

; "MA"
.export tunix_balloc
.proc tunix_balloc
    jsr balloc
    beq :+
    jmp respond
:   jmp respond_error
.endproc

; "MFb"
syscall1 tunix_bfree, bfree, ldx

; "MI"
syscall0 tunix_mem_info, mem_info

;; PROCESSES

; "P?"
.export tunix_procs
.proc tunix_procs
    lda FNLEN
    cmp #1
    bne :+
    jmpa respond1, pid
:   lda filename+1
    cmp #'F'
    beq tunix_fork
    cmp #'E'
    beq tunix_exit
    cmp #'K'
    beq tunix_kill
    cmp #'W'
    beq tunix_wait
    cmp #'S'
    beq tunix_suspend
    cmp #'R'
    beq tunix_resume
    cmp #'L'
    beq tunix_proc_list
    cmp #'I'
    beq tunix_proc_info
    jmp respond_error
.endproc

syscall0 tunix_fork, fork
syscall1 tunix_wait, wait, ldx
syscall1 tunix_suspend, suspend, ldx
syscall1 tunix_resume, resume, ldx
syscall1 tunix_exit, exit, lda
syscall1v tunix_proc_info, proc_info, ldx
syscall0 tunix_proc_list, proc_list

; "PK"
.export tunix_kill
.proc tunix_kill
    ldx SA
    ldy FNLEN
    lda #255
    cpy #3
    bne :+
:   jsra kill, filename+2
    bcs :+
    jmp respond_ok
:   jmp respond_error
.endproc

;;; DRIVER REGISTRATION

; "D?"
.export tunix_drivers
.proc tunix_drivers
    lda filename+1
    cmp #'R'
    beq r
    cmp #'A'
    beq ai
    cmp #'C'
    beq co
    cmp #'F'
    beq fi
    jmp respond_error
r:  jmp tunix_register
ai: jmp tunix_iopage_alloc
co: jmp tunix_iopage_commit
fi: jmp tunix_iopage_free
.endproc

;;;;;;;;;;;;
;;; INIT ;;;
;;;;;;;;;;;;
;
; Booting the thing.

    .zeropage

bnk:            .res 1
col:            .res 1

    .segment "KERNEL"

.export init_ultimem
.proc init_ultimem
    ; Unhide UltiMem registers
    lda $9f55
    lda $9faa
    lda $9f01
    lda $9ff3
    cmp #$11
    beq has_ultimem
    error txt_no_ultimem

    ;; Write banks with pattern.
has_ultimem:
    mvb bnk, #FIRST_BANK
start_bank_write:
    stwi zp1, $a000
    mvb blk5, bnk

write_byte:
    ldy #0
    mvb {(zp1),y}, bnk
    iny
    mvb {(zp1),y}, bnk
    inc zp1
    inc zp1
    bne write_byte
    inc zp1+1
    lda zp1+1
    cmp #$a1    ; Just one page.
    bne write_byte
    ; Next bank
    inc bnk
    lda bnk
    cmp #MAX_BANKS
    bne start_bank_write

    ;; Read banks and check pattern.
    lda #0
    sta col
    sta banks_ok
    sta banks_faulty
    mvb bnk, #FIRST_BANK

start_bank_read:
    stwi zp1, $a000
    mvb blk5, bnk

read_byte:
    ldy #0
    lda bnk
    cmp (zp1),y
    bne uerror  ; Eek!
    iny
    lda bnk
    cmp (zp1),y
    bne uerror  ; Eek!

    inc zp1
    inc zp1
    bne read_byte
    inc zp1+1
    lda zp1+1
    cmp #$a1    ; Just one page.
    bne read_byte

    inc banks_ok
    ldx bnk
    lpush_x banks, free_bank
    jsra printbnk, #'.'

next_bank:
    inc bnk
    lda bnk
    cmp #MAX_BANKS
    bne start_bank_read

    lda banks_faulty
    bne has_errors
    print txt_ram_ok
    jsr print_free_ram
    jmp print_cr

uerror:
    inc banks_faulty
    jsra printbnk, #'!'
    jmp next_bank
.endproc

.export bank_last
bank_last = banks + $7f

has_errors:
    jsr print_decbyte
    print txt_faulty_banks

print_free_ram:
    lda banks_ok
print_free_ram_a:
    jsr print_decbyte_x8
    print txt_banks_free
    rts

.export printbnk
.proc printbnk
    jsr BSOUT
    inc col
    lda col
    cmp #16
    bne r
    jsra BSOUT, #13
    mvb col, #0
r:  rts
.endproc

    .segment "KERNELDATA"

txt_no_ultimem:
  .byte "NO ULTIMEM/VIC-MIDIFOUND."
  .byte 13, 0
txt_faulty_banks:
  .byte " FAULTY BANKS.", 13, 0
txt_ram_ok:
  .byte 13, "RAM OK.", 13, 0
txt_banks_free:
  .byte "K RAM FREE.", 13, 0

vec_localcode_reloc:
  .word __LOCALCODE_LOAD__
  .word __LOCALCODE_RUN__
  .word __LOCALCODE_SIZE__
vec_backup_kernal:
  .word IOVECTORS
  .word old_kernal_vectors
  .word IOVECTOR_SIZE
vec_tunix_kernal:
  .word tunix_vectors
  .word IOVECTORS
  .word IOVECTOR_SIZE

clr_globalbss:
  .word __GLOBALBSS_RUN__
  .word __GLOBALBSS_SIZE__
clr_localbss:
  .word __LOCALBSS_RUN__
  .word __LOCALBSS_SIZE__ - __ULTIMEM_SIZE__
clr_localbss2:
  .word __LOCALBSS2_RUN__
  .word __LOCALBSS2_SIZE__
clr_lbanks:
  .word lbanks, MAX_BANKS
clr_lbanksb:
  .word lbanksb, MAX_BANKS

    .segment "KERNEL"

.export clear_bss_segments
.proc clear_bss_segments
    sbzeroax clr_globalbss
    sbzeroax clr_localbss
    sbzeroax clr_localbss2
    rts
.endproc

.export init_data
.proc init_data
    ;;; Init lists.
    ;; Link
    ldx #0
@l: txa
    beq :++
    ; Pointers to previous elements.
    sec
    sbc #1
    cpx #MAX_PROCS
    bcs :+
    sta procsb,x
:   cpx #MAX_IOPAGES
    bcs :+
    sta iopagesb,x
    ; Pointers to next elements.
:   txa
    clc
    adc #1
    sta glfns,x
    sta lfns,x
    cpx #MAX_PROCS - 1
    bcs :+
    sta procs,x
    sta waiting,x
:   cpx #MAX_DRVS - 1
    bcs :+
    sta drvs,x
:   cpx #MAX_IOPAGES - 1
    bcs :+
    sta iopages,x
    mvb {iopage_pid,x}, #255
:   inx
    bne @l
    ;; Finish up.
    lda #1
    sta free_glfn
    sta free_proc
    sta free_wait
    sta free_iopage
    sta free_drv
    rts
.endproc

.export make_proc0
.proc make_proc0
    ;;; Make init process 0.
    print txt_starting_multitasking
    inc multitasking
    ;; Make holograhic process to fork.
    ; Unlink from free list.
    mvb procs, #0 ; (Running alone.)
    ; Fill in banks for process 0.
    mvb proc_ram123+1, ram123
    sta proc_data+1
    mvb proc_io23+1, io23
    mvb proc_blk1+1, blk1
    sta tunix_blk1
    mvb proc_blk2+1, blk2
    sta tunix_blk2
    mvb proc_blk3+1, blk3
    mvb proc_blk5+1, blk5

    ;; Fork process 0.
    inc pid
    jsry machdep_fork, #0
    dec pid

    ; Move old BLK1 with new procdata
    ; to new.
    push blk3
    push blk5
    mvb blk3, blk1
    mvb blk5, proc_blk1
    push blk2
    jsra next_speedcopy, speedcopy_blk3_to_blk5
    pop blk2
    mvb blk3, blk2
    mvb blk5, proc_blk2
    push blk2
    jsra next_speedcopy, speedcopy_blk3_to_blk5
    pop blk2
    pop blk5
    pop blk3

    ; Continue with forked banks.
    mvb blk1, proc_blk1
    mvb blk2, proc_blk2
    mvb proc_flags, #PROC_RUNNING
    mvb io23, proc_io23
    mvb tunix_blk1, proc_blk1
    mvb tunix_blk2, proc_blk2
    mvb ram123, proc_data
    mvb blk3, proc_blk3
    mvb blk5, proc_blk5

    ; Remove lbank entries forever.
    sbzeroax clr_lbanks
    sbzeroax clr_lbanksb
    rts
.endproc

.export register_syscall_driver
.proc register_syscall_driver
    print txt_starting_syscalls
    smemcpyax vec_backup_kernal
    mvw old_load, old_kernal_vectors + IDX_LOAD
    mvw old_save, old_kernal_vectors + IDX_SAVE
    stwi old_kernal_vectors + IDX_LOAD, kernal_load
    stwi old_kernal_vectors + IDX_SAVE, kernal_save
    smemcpyax vec_tunix_kernal ; Replace

    ;; Make devices default to KERNAL.
    mvb drv_vl, #<old_kernal_vectors
    mvb drv_vh, #>old_kernal_vectors

    ;; Register TUNIX device.
    ldaxi tunix_driver
    ldy #TUNIX_DEVICE
    jmp register
.endproc

.export boot
.proc boot
    jsr FRESTOR
    print txt_tunix
    smemcpyax vec_localcode_reloc
    jsr clear_bss_segments
    jsr init_data
    jsr init_ultimem
    jsr gen_speedcodes
    jsr make_proc0
    jsr register_syscall_driver
.ifdef EARLY_TESTS
    jsr tests
.endif ; .ifdef EARLY_TESTS

    print txt_init
    jsr lib_fork
    cmp #0
    beq :+
    jmp proc0

:   jsr lib_proc_list

    ; Welcome messages with free RAM.
:   ldayi txt_welcome
    jsr PRTSTR
    ldaxi banks
    jsry list_length, free_bank
    txa
    jsr print_free_ram_a
    jsr print_cr

    ; Move on to BASIC.
    jsr basic_cold_init
.ifdef START_INIT
    jsr start_init
.endif
    ;jmp get_ready
.endproc

.export get_ready
.proc get_ready
    ldx #0
    ldx #$f8
    txs
    jmp READY
.endproc

.export basic_cold_init
.proc basic_cold_init
    ;; BASIC cold start.
    ldayi $e437
    jsr PRTSTR
    jsr INITVCTRS
    jsr INITBA
    jmp $e412
.endproc

.ifdef START_INIT
.export start_init
.proc start_init
    sei
    ldx #0
l:  lda loadkeys,x
    beq :+
    sta KEYD,x
    inx
    bne l   ; (jmp)
:   stx NDX
    cli
    rts
.endproc
.endif

    .segment "KERNELDATA"

.export txt_welcome
txt_welcome:
  .byte 13 ; PETSCII_CLRSCR
  .byte "TUNIX - ", 0

loadkeys:
  .byte "LOAD", '"', "INIT", '"', ",8"
  .byte 13, "RUN", 13, 0

txt_tunix:
  .byte PETSCII_CLRSCR, $8e
  .byte "BOOTING TUNIX.", 13, 0
txt_starting_multitasking:
  .byte "STARTING MULTITASKING.", 13, 0
txt_starting_syscalls:
  .byte "STARTING SYSCALLS.", 13, 0
txt_init:
  .byte "STARTING INIT.", 13, 0

tunix_vectors:
.word open, close, chkin, ckout, clrchn
.word basin, bsout, stop, getin, clall
.word usrcmd, load, save, blkin, bkout

;;;;;;;;;;;;;;;;;
;;; DISPATCH ;;;;
;;;;;;;;;;;;;;;;;
;
; KERNAL I/O handlers with resident
; parts in IO23 that bank in BLK1
; (this place right here).

    .segment "KERNEL"

.export call_glfn_driver
.proc call_glfn_driver
    ldy glfn_drv,x
.endproc

; Call driver
; Y: driver
; A: vector offset
.export call_driver
.proc call_driver
    ; (vector base + A).
    clc
    adc drv_vl,y
    sta g+1
    sta h+1
    lda drv_vh,y
    adc #0
    sta g+2
    sta h+2
    inc h+1
    bne g
    inc h+2
g:  lda $ffff
    sta call_driver3+1
h:  lda $ffff
    sta call_driver3+2

    ;; Bank in driver BLK1.
    ldx pid
    cpy #0
    beq :+
    ldx drv_pid,y
:   ldy proc_blk1,x
    jmp call_driver1
.endproc

.export open2
.proc open2
    stwi FNADR, filename
    push LFN
    jsr lfn_to_glfn
    sta LFN
    tax
    mvb {glfn_sa,x}, SA
    mvb {glfn_dev,x}, DEV
    tay
    mvb {glfn_drv,x}, {dev_drv,y}
    tay
    jsra call_driver, #IDX_OPEN
    pop LFN
    popw FNADR
    jmp tunix_leave
.endproc

.export chkin2
.proc chkin2
    ldx reg_x
    lda lfn_glfn,x
    beq :+
    tax
    mvb SA, {glfn_sa,x}
    mvb DFLTN, {glfn_dev,x}
    clc
    bcc :++
:   sec
:   php
    pla
    sta flags
    jmp tunix_leave
.endproc

.export ckout2
.proc ckout2
    ldx reg_x
    lda lfn_glfn,x
    beq :+
    tax
    mvb SA, {glfn_sa,x}
    mvb DFLTO, {glfn_dev,x}
    clc
    bcc :++
:   sec
:   php
    pla
    sta flags
    jmp tunix_leave
.endproc

.macro iohandler name2, device, drvop
    .export name2
    .proc name2
        ldx device
        ldy dev_drv,x
        jsra call_driver, #drvop
        jmp tunix_leave
    .endproc
.endmacro

iohandler basin2, DFLTN, IDX_BASIN
iohandler getin2, DFLTN, IDX_GETIN
iohandler bsout2, DFLTO, IDX_BSOUT
iohandler blkin2, DFLTN, IDX_BLKIN
iohandler bkout2, DFLTO, IDX_BKOUT

.export close2
.proc close2
    jsr lfn_to_glfn
    sta reg_a
    mvb SA, {glfn_sa,x}
    ldy glfn_drv,x
    mvb {glfn_drv,x}, #0
    tya
    tax
    jsra call_glfn_driver, #IDX_CLOSE
    jmp tunix_leave
.endproc

.export clall2
.proc clall2
    ldx first_lfn
    beq :++
:   phx
    jsr close
    plx
    lloop_x lfns, :-
:   jmp tunix_leave
.endproc

.export stop2
.proc stop2
    ldy #0
    jsra call_driver, #IDX_STOP
    jmp tunix_leave
.endproc

.export usrcmd2
.proc usrcmd2
    ldy #0
    jsra call_driver, #IDX_STOP
    jmp tunix_leave
.endproc

;;;;;;;;;;;;;;;;;
;;; SCHEDULER ;;;
;;;;;;;;;;;;;;;;;

; Schedule task switch
; Picks next or first on running list.
.export schedule
.proc schedule
    lda multitasking
    beq r

    ;; Get next running.
    ldx pid
    lda procs,x
    and #PROC_RUNNING | PROC_BABY
    bne ok

    ;; Restart list.
    ; Check if proc 0 is running.
    lda proc_flags + 0
    cmp #PROC_RUNNING ; (never a baby)
    bne :+
    lda #0
    beq ok ; (jmp)
    ; Use first running.
:   lda running

    ; Switch.
ok: cmp pid
    beq r ; (Don't switch to self.)
    tay
    push ram123
    push blk2
    push blk3
    push blk5
    sei
    jsr switch ; Switch IO23 & lowmem.
    cli
    pop blk5
    pop blk3
    pop blk2
    pop ram123

r:  rts

ouch:
    sta tmp1
    error err_first_not_running
.endproc

err_first_not_running:
    .byte "FIRST NOT RUNNING.", 0

; Switch internal RAM and IO23 only(!).
;
; The process being switched to will
; never see this function but return
; from either fork() or switch(),
; depending on which was called before.
;
; Y: Process ID
.export switch
.proc switch
    ;;; Save current.
    tsx
    cpx #STACK_FENCE
    bcs :+
    jsr stack_overflow
    ; NOT REACHED.
:   stx stack
    ldx pid
    mvb blk5, {proc_io23,x}
    save_internal_ram_to_blk5

    ;;; Load next.
    mvb blk5, {proc_io23,y}
    ;; Color, screen and VIC.
    smemcpyax vec_blk5_to_color
    smemcpyax vec_blk5_to_screen
    smemcpyax vec_blk5_to_vic
    ;; Low memory (incl. stack).
    mvb blk2, speedcopy_blk5_to_lowmem
    ; Set return address of speed copy
    ; manually as we cannot use the
    ; stack since its just about to be
    ; overwritten.
    mvb $5801, #<:+
    mvb $5802, #>:+
    jmp $4000
:   mvb blk2, tunix_blk2

    ;; Hop over.
    mvb io23, blk5
    ldx stack
    txs
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; KERNAL I/O handlers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

    .segment "LOCALCODE"

.export call_driver1
.proc call_driver1
    push blk1
    sty blk1
    load_regs
.endproc

.export call_driver3
.proc call_driver3
    jsr $fffe
    store_regs_and_flags
    pop blk1
    rts
.endproc

; Restore banks on BLK1 & RAM123.
.export tunix_leave
.proc tunix_leave
    jsr schedule

    ; Pre-fetch process flags.
    ldx pid
    ldy proc_flags,x

    ; Restoring its banks.
    pop blk2
    pop blk1
    pop ram123
    cpy #PROC_BABY
    beq grow_baby

    ; Restore syscall return values.
:   lda flags
    pha
    load_regs
    plp
    rts

.export grow_baby
grow_baby:
    mvb blk1,tunix_blk1
    mvb blk2,tunix_blk2
    mvb {proc_flags,x}, #PROC_RUNNING
    mvb ram123, {proc_ram123,x}
    lda proc_blk1,x
    ldy proc_blk2,x
    sta blk1
    sty blk2
    jmp :-
.endproc

.export tunix_enter
.proc tunix_enter
    store_regs
.endproc

; Map in RAM123 and BLK1.
.export tunix_enter2
.proc tunix_enter2
    popw fnord
    ldx pid
    push ram123
    push blk1
    push blk2
    pushw fnord
    mvb blk1, tunix_blk1
    mvb blk2, tunix_blk2
    mvb ram123, {proc_data,x}
    rts
.endproc

fnord: .res 2

.export open
.proc open
    store_regs

    ;; Move filename + pointer to IO23.
    ldy FNLEN
    beq :++
    dey
:   mvb {filename,y}, {(FNADR),y}
    dey
    bpl :-
:   jsr tunix_enter2
    pushw FNADR
    jmp open2
.endproc

.macro iowrap name, name2
    .export name
    .proc name
        jsr tunix_enter
        jmp name2
    .endproc
.endmacro

iowrap chkin, chkin2
iowrap ckout, ckout2
iowrap basin, basin2
iowrap bsout, bsout2
iowrap getin, getin2
iowrap blkin, blkin2
iowrap bkout, bkout2
iowrap close, close2
iowrap clall, clall2
iowrap stop, stop2
iowrap usrcmd, usrcmd2

.export clrchn
.proc clrchn
    ; TODO: Call all LFNs.
    jsr tunix_enter
    ldy #0
    jsra call_driver, #IDX_CLRCHN
    jmp tunix_leave
.endproc

.macro blkiohandler name, idx
    .export name
    .proc name
        jsr tunix_enter
        ldx DEV
        ldy dev_drv,x
        jsra call_driver, idx
        jmp tunix_leave
    .endproc
.endmacro

blkiohandler load, #IDX_LOAD
blkiohandler save, #IDX_SAVE

;;;;;;;;;;;;;;;;;;;;;
;;; KERNAL DRIVER ;;;
;;;;;;;;;;;;;;;;;;;;;

    .segment "LOCALCODE"

.export kernal_block
.proc kernal_block
    ; Restore process banks.
    ldx pid
    enter_banks_x
    load_regs
.endproc
.export kernal_block2
.proc kernal_block2
    jsr $ffff
    store_regs_and_flags
    leave_banks
    load_regs
    rts
.endproc

.export kernal_load
.proc kernal_load
    mvw kernal_block2 + 1, old_load
    jmp kernal_block
.endproc

.export kernal_save
.proc kernal_save
    mvw kernal_block2 + 1, old_save
    jmp kernal_block
.endproc

;;;;;;;;;;;;;
;;; TESTS ;;;
;;;;;;;;;;;;;
;
; Tests running before regular boot.

.ifdef EARLY_TESTS

    .segment "TESTS"

txt_tests:
  .byte "RUNNING TESTS.", 13, 0
txt_testing_data:
  .byte "TESTING DATA.", 13, 0
txt_testing_processes:
  .byte "TESTING PROCS.", 13, 0
txt_child:
  .byte "BABY.", 13, 0
txt_hyperactive_child:
  .byte ":):):):):):):):):):):):)", 0
txt_tests_passed:
  .byte "!!! CHECKS PASSED !!!", 13, 0

note_test_syscall:
  .byte "TESTING SIMPLE SYSCALL", 13, 0
note_forking:
  .byte "FORKING.", 13, 0
note_waiting_for_child:
  .byte "WAITING FOR CHILD.", 13, 0
note_child_exited:
  .byte "CHILD EXITED.", 13, 0
note_getting_pid:
  .byte "GETTING PID.", 13, 0
note_forking_hyperactive:
  .byte "FORKING HYPERACTIVE CHILD."
  .byte 13, 0
note_killing_hyperactive:
  .byte "KILLING HYPERACTIVE.", 13, 0
note_waiting_for_hyperactive:
  .byte "WAITING FOR HYPERACTIVE TO "
  .byte "EXIT.", 13, 0
note_iopage_alloc:
  .byte "ALLOCATING I/O PAGE.", 13, 0
note_iopage_commit:
  .byte "COMMITTING I/O PAGE.", 13, 0
note_iopage_free:
  .byte "FREE I/O PAGE.", 13, 0

err_free_banks_after_init:
  .byte "WRONG TOTAL # OF FREE BANKS."  
  .byte 0
err_free_banks_after_free:
  .byte "WRONG # OF BANKS AFTER "
  .byte "FREEING ONE AGAIN.", 0
err_wrong_glfn_order:
  .byte "WRONG GLFN ORDER.", 0
err_wrong_deque_index:
  .byte "UNEXPECTED DEQUE INDEX OF "
  .byte "FIRST ALLOCATED ONE.", 0
err_bad_lbanks:
  .byte "BAD LBANKS.", 0
err_bad_lbanksb:
  .byte "BAD LBANKSB.", 0
err_bad_lbanks_rm:
  .byte "BAD LBANKS AFTER REMOVAL.", 0
err_bad_lbanksb_rm:
  .byte "BAD LBANKSB AFTER REMOVAL.", 0
err_lbanks_not_empty:
  .byte "LBANKS/LBANKSB NOT EMPTY.", 0
err_first_lbank_not_0:
  .byte "FIRST LBANK NOT 0 FOR ENPTY "
  .byte "DEQUE.", 0
err_wrong_free_proc_count:
  .byte "WRONG # OF FREE PROCS.", 0
err_cannot_fork:
  .byte "CANNOT FORK.", 0
err_num_lbanks_after_fork:
  .byte "WRONG # OF LBANKS AFTER FORK."
  .byte 0
err_child_running_after_exit:
  .byte "CHILD STILL RUNNING AFTER "
  .byte "EXIT.", 0
err_init_pid_not_0:
  .byte "INIT PID NOT 0 AFTER FORK.", 0
err_cannot_kill:
  .byte "CANNOT KILL.", 0
err_cannot_wait:
  .byte "CANNOT WAIT.", 0
err_expected_iopage_base:
  .byte "I/O PAGE NOT AT IOPAGE BASE."
  .byte 0
err_cannot_commit_iopage:
  .byte "CANNOT COMMIT I/O PAGE.", 0
err_cannot_free_iopage:
  .byte "CANNOT FREE I/O PAGE.", 0
err_wrong_num_free_iopages:
  .byte "WRONG # OF FREE I/O PAGES.", 0

.export expected_lbanks
.export expected_lbanksb
.export expected_lbanks_rm
.export expected_lbanksb_rm

expected_lbanks:
  .byte 0, 0, 1, 2
expected_lbanksb:
  .byte 0, 2, 3, 0
expected_lbanks_rm:
  .byte 0, 0, 0, 1
expected_lbanksb_rm:
  .byte 0, 3, 0, 0

; TODO: Make a proper formula from
; defined constants.
FREE_BANKS_AFTER_INIT = MAX_BANKS - FIRST_BANK - 7 - 7 - 3

.proc clear_lbanks
    stwi d, lbanks
    stwi c, MAX_BANKS
    jsr bzero
    mvb first_lbank, #0
    rts
.endproc

.export tests_data
.proc tests_data
    print txt_testing_data

    ;;;;;;;;;;;;;;;;;;;;;;
    ;;; LISTS & DEQUES ;;;
    ;;;;;;;;;;;;;;;;;;;;;;

    ;; Total free banks.
    ldaxi banks
    jsry list_length, free_bank
    cpx #FREE_BANKS_AFTER_INIT
    beq :+
    error err_free_banks_after_init

    ;; Doubly used list arrays.
    ; Pop bank from free list.
:   jsr balloc
    jsr bfree
    ldaxi banks
    jsry list_length, free_bank
    cpx #FREE_BANKS_AFTER_INIT
    beq :+
    error err_free_banks_after_free

    ;; Deque
    ; Allocate first free proc.
:   lpop_x procs, free_proc
    phx
    ldaxi procs
    jsry list_length, free_proc
    cpx #MAX_PROCS - 2 ; (+ init)
    beq :+
    error err_wrong_free_proc_count
:   plx
    ; Push free proc onto running.
    dpush_x procs, procsb, running
    ldaxi procs
    jsry list_length, running
    cpx #1
    beq :+
    error err_wrong_deque_index
    ; Move it back to free procs.
:   drm_x procs, procsb, running
    dpush_x procs, procsb, free_proc
    jsr clear_lbanks
    ldx #1
    alloc_lbank_x
    ldx #2
    alloc_lbank_x
    ldx #3
    alloc_lbank_x

    stwi s, lbanks
    stwi d, expected_lbanks
    stwi c, 4
    jsr cmpmem
    beq :+
    error err_bad_lbanks
:   stwi s, lbanksb
    stwi d, expected_lbanksb
    stwi c, 4
    jsr cmpmem
    beq :+
    error err_bad_lbanksb

:   ldx #2
    free_lbank_x
    stwi s, lbanks
    stwi d, expected_lbanks_rm
    stwi c, 4
    jsr cmpmem
    beq :+
    error err_bad_lbanks_rm
:   stwi s, lbanksb
    stwi d, expected_lbanksb_rm
    stwi c, 4
    jsr cmpmem
    beq :+
    error err_bad_lbanksb_rm

:   ldx #1
    free_lbank_x
    ldx #3
    free_lbank_x
    ldx #0
:   lda lbanks,x
    bne :+
    lda lbanksb,x
    bne :+
    inx
    cpx #MAX_BANKS
    bne :-
    beq :++
:   error err_lbanks_not_empty

:   lda first_lbank
    beq :+
    error err_first_lbank_not_0

:   rts
.endproc

.export tests_processes
.proc tests_processes
    print txt_testing_processes

    ;; Fork and wait for child to exit.
    print note_forking
    jsr lib_getpid
    jsr lib_proc_info
    jsr lib_fork
    bcc :+
    error err_cannot_fork
:   pha
    cmp #0
    bne :+
    jmp baby
:   ; Wait for child to exit.
    print note_waiting_for_child
    pla
    jsr lib_wait
    print note_child_exited

    ;; Check our process ID.
    print note_getting_pid
    jsr lib_getpid
    cmp #0
    beq :+
    error err_init_pid_not_0

    ;; Fork, kill, then wait for child.
:   print note_forking_hyperactive
    jsr lib_fork
    bcc :+
    error err_cannot_fork
:   cmp #0
    bne :+
    jmp hyperactive_child
    ; Kill child.
:   pha
    print note_killing_hyperactive
    pla
    pha
    jsr lib_kill
    bcc :+
    error err_cannot_kill
    ; Wait for child
:   print note_waiting_for_hyperactive
    pla
    jsr lib_wait
    bcc :+
    error err_cannot_wait
:   rts
.endproc

.export tests_iopages
.proc tests_iopages
    ldaxi iopages
    jsry list_length, free_iopage
    cpx #MAX_IOPAGES - 1
    beq :+
    error err_wrong_num_free_iopages

:   print note_iopage_alloc
    jsr lib_iopage_alloc
    cmp #IOPAGE_BASE
    beq :+
    error err_expected_iopage_base

:   pha
    ldaxi iopages
    jsry list_length, free_iopage
    cpx #MAX_IOPAGES - 2
    beq :+
    error err_wrong_num_free_iopages

:   pla
    pha
    print note_iopage_commit
    pla
    pha
    jsr lib_iopage_commit
    bcc :+
    error err_cannot_commit_iopage

:   pla
    pha
    ldaxi iopages
    jsry list_length, free_iopage
    cpx #MAX_IOPAGES - 2
    beq :+
    error err_wrong_num_free_iopages

:   pla
    pha
    print note_iopage_free
    pla
    jsr lib_iopage_free
    bcc :+
    error err_cannot_free_iopage

:   ldaxi iopages
    jsry list_length, free_iopage
    cpx #MAX_IOPAGES - 1
    beq :+
    error err_wrong_num_free_iopages
:   rts
.endproc

.export tests
.proc tests
    print txt_tests
    jsr tests_data
    print note_test_syscall
    jsr lib_schedule
    jsr tests_processes
    jsr tests_iopages
    print txt_tests_passed
    rts
.endproc

.export baby
.proc baby
    print txt_child
    ldaxi lbanks
    jsry list_length, first_lbank
    cpx #10
    beq :+
    error err_num_lbanks_after_fork
:   jsr lib_exit
    error err_child_running_after_exit
.endproc

.export hyperactive_child
.proc hyperactive_child
:   print txt_hyperactive_child
    jmp :-
.endproc

.endif ; .ifdef EARLY_TESTS
