 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;                                ;;;
 ;;; ###### ##  ## ####   ## ##  ## ;;;
 ;;;   ##   ##  ## ##  ## ##   ##   ;;;
 ;;;   ##   ###### ##  ## ## ##  ## ;;;
 ;;;                                ;;;
 ;;; Multi-tasking KERNAL extension ;;;
 ;;;  (Commodore VIC-20 + UltiMem)  ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    jmp start

;;; Compile-time

__VIC20__       = 1
EARLY_TESTS     = 1
BLEEDING_EDGE   = 1

;;; Linker

.import __BOOT_LOAD__
.import __BOOT_RUN__
.import __BOOT_SIZE__
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

.export FRESTOR, READST, SETLFS, SETNAM
.export OPEN, CLOSE, CHKIN, CKOUT
.export CLRCHN, BASIN, BSOUT, LOAD
.export SAVE, SETTIM
.export RDTIM, STOP, GETIN, CLALL

PETSCII_CLRSCR = 147

FRESTOR = $fd52

STATUS  = $90
DFLTN   = $99
DFLTO   = $9a
FNLEN   = $b7
LFN     = $b8
SA      = $b9
DEV     = $ba
FNADR   = $bb

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
MAX_PROCS    = 64
MAX_SIGNALS  = 64
MAX_DRVS     = 16
MAX_DEVS     = 32
MAX_DRV_NAME = 8    ; This too.

;;; MACHDEP

IOPAGE_BASE  = $9b
MAX_IOPAGES  = 4
STACK_LIMIT  = 16

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

tmp1:   .res 2
tmp2:   .res 2
tmp3:   .res 2

ptr1:   .res 2

;;; Extended memory banks
banks:      .res MAX_BANKS
free_bank:  .res 1
bank_refs:  .res MAX_BANKS

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
glfns:      .res MAX_LFNS
glfn_refs:  .res MAX_LFNS
;; Last parameters to OPEN.
glfn_drv:   .res MAX_LFNS

;;; Processes
procs:      .res MAX_PROCS
procsb:     .res MAX_PROCS
free_proc:  .res 1
running:    .res 1
sleeping:   .res 1
zombie:     .res 1
PROC_BABY       = 16
PROC_ZOMBIE     = 32
PROC_RUNNING    = 64
PROC_SLEEPING   = 128
proc_flags: .res MAX_PROCS
exit_codes: .res MAX_PROCS
;; Current banks.
; Kernel and IO pages (per-process)
proc_io23:  .res MAX_PROCS
; Shadow RAM123 (per-process)
proc_data:  .res MAX_PROCS
; Process RAM123
proc_ram123:.res MAX_PROCS
proc_blk1:  .res MAX_PROCS
proc_blk2:  .res MAX_PROCS
proc_blk3:  .res MAX_PROCS
proc_blk5:  .res MAX_PROCS

;;; Drivers
drvs:       .res MAX_DRVS + 1
drv_pid:    .res MAX_DRVS + 1
drv_dev:    .res MAX_DRVS + 1
drv_vl:     .res MAX_DRVS + 1
drv_vh:     .res MAX_DRVS + 1
drv_names:  .res MAX_DRVS * MAX_DRV_NAME

;;; Drivers assigned to devices.
dev_drv:    .res MAX_DEVS

;;; Initial speed code banks.
speedcopy_blk3_to_blk5:   .res 1
speedcopy_blk5_to_lowmem: .res 1
speedcopy_lowmem_to_blk5: .res 1

;;; KERNAL
old_kernal_vectors: .res 32

;;;;;;;;;;;;;
;;; LOCAL ;;;
;;;;;;;;;;;;;
;
; Per process.

    .segment "LOCALBSS"

.export tunix_io23, tunix_blk1, lbanks
.export lbanksb, first_lbank, lfns
.export lfnsb, lfn_glfn, first_lfn
.export waiting, waitingb, waiting_pid
.export free_wait, first_wait, pid
.export reg_a, reg_x, reg_y, stack
.export flags, saved_vic

;; Vitals
tunix_io23:     .res 1  ; Per process.
tunix_blk1:     .res 1  ; Same for all.
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
first_wait:     .res 1

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
signals:          .res MAX_SIGNALS
signalsb:         .res MAX_SIGNALS
signal_type:      .res MAX_SIGNALS
signal_payload:   .res MAX_SIGNALS
signal_handler_l: .res MAX_SIGNALS
signal_handler_h: .res MAX_SIGNALS
pending_signal_types: .res 256
free_signal:          .res 1
pending_signal:       .res 1

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

.macro save_regs
    sta reg_a
    stx reg_x
    sty reg_y
.endmacro

.macro load_regs
    lda reg_a
    ldx reg_x
    ldy reg_y
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

.macro jsra to, val
    lda val
    jsr to
.endmacro

.macro jmpa to, val
    lda val
    jmp to
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
; 'lpop*' and 'lloop*' also work with
; deques.
;
; There may be either multiple lists or
; deques in one set of arrays.

;; Pop from front of list.

.macro lpopx list, free
    ldx free
    lda list,x
    sta free
.endmacro

.macro lpopy list, free
    ldy free
    lda list,y
    sta free
.endmacro

; Push to front of list.

.macro lpushx list, first
    lda first
    sta list,x
    stx first
.endmacro

.macro lpushy list, first
    lda first
    sta list,y
    sty first
.endmacro

;; Move between lists.
.macro lmovex list, from, to
    lpopy list, from
    lpushx list, to
.endmacro

.macro lmovey list, from, to
    lpopy list, from
    lpushy list, to
.endmacro

;; Forwards iteration.

.macro lloopx list, loop
    lda list,x
    tax
    bne loop
.endmacro

.macro lloopy list, loop
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

.macro dpushx fw, bw, first
    lda first
    sta fw,x
    lda #0
    sta bw,x
    stx first
.endmacro

.macro dpushy fw, bw, first
    lda first
    sta fw,y
    lda #0
    sta bw,y
    sty first
.endmacro

;; Remove from deque.

.macro drmx fw, bw, first
    phx
    cpx first
    bne :+
    lda fw,x
    sta first
:   ; Link previous
    lda bw,x
    beq :+
    tay
    lda fw,x
    sta fw,y
    ; Link next
:   tax
    beq :+
    tya
    sta bw,x
:   plx
.endmacro

.macro drmy fw, bw, first
    phy
    cpy first
    bne :+
    lda fw,y
    sta first
:   ; Link previous
    lda bw,y
    beq :+
    tax
    lda fw,y
    sta fw,x
    ; Link next
:   tay
    beq :+
    txa
    sta bw,y
:   ply
.endmacro

;; Allocate item in deque.
;
; Pops item from front of a 'free' list
; and pushes it onto the front of an
; 'allocated' list in the same deque
; array.

.macro dallocx fw, bw, from, to
    lpopx fw, from
    dpushx fw, bw, to
.endmacro

.macro dallocy fw, bw, from, to
    lpopy fw, from
    dpushy fw, bw, to
.endmacro

;; Move between deques
;
; Also used to move items back to free
; lists.

.macro dmovex fw, bw, from, to
    drmx fw, bw, from
    dpushx fw, bw, to
.endmacro

.macro dmovey fw, bw, from, to
    drmy fw, bw, from
    dpushy fw, bw, to
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCESS LIST MACROS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Allocate free process.

.macro alloc_proc_running_y
    dallocy procs, procsb, free_proc, running
.endmacro

;; Move between running to sleeping.

.macro mv_running_sleeping_x
    dmovex procs, procsb, running, sleeping
.endmacro

.macro mv_sleeping_running_x
    dmovex procs, procsb, sleeping, running
.endmacro

;; Processes waiting for others to exit.

; Allocate local slot in waiting list.
.macro alloc_waiting_y
    dallocy waiting, waitingb, free_wait, first_wait
.endmacro

; Remove process from waiting list.
.macro rm_waiting_y
    dmovey waiting, waitingb, first_wait,free_wait
.endmacro

; Make zombie a free process.
.macro reap_zombie_x
    dmovex procs, procsb, zombie, free_proc
.endmacro

;;;;;;;;;;;;;;;;;;;;;
;;; SIGNAL MACROS ;;;
;;;;;;;;;;;;;;;;;;;;;

.macro alloc_signal_y
    dallocy signals, signalsb, free_signal, pending_signal
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IO PAGE LIST MACROS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

.macro alloc_iopage_x
    dallocx iopages, iopagesb, free_iopage, first_iopage
.endmacro

.macro free_iopage_x
    dmovex iopages, iopagesb, first_iopage, free_iopage
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCESS BANK MACROS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

.macro get_procblk_x proc, blk
    lda proc,x
    sta blk
.endmacro

.macro get_procblk_y proc, blk
    lda proc,y
    sta blk
.endmacro

.macro io23x
    get_procblk_x proc_io23, io23
.endmacro

.macro io23y
    get_procblk_y proc_io23, io23
.endmacro

.macro io23x_at_blk5
    get_procblk_x proc_io23, blk5
.endmacro

.macro io23y_at_blk5
    get_procblk_y proc_io23, blk5
.endmacro

.macro datax
    get_procblk_x proc_data, ram123
.endmacro

.macro datay
    get_procblk_y proc_data, ram123
.endmacro

;;;;;;;;;;;;;;;;;;;;;
;;; SHADOW RAM123 ;;;
;;;;;;;;;;;;;;;;;;;;;

.macro enter_data_x
    push ram123
    datax
.endmacro

.macro enter_data_y
    push ram123
    datay
.endmacro

.macro leave_data
    pop ram123
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     PROCESS CONTEXT    ;;;
;;; (SHADOW RAM123 + IO23) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.macro enter_tunix_x
    push io23
    io23x
    enter_data_x
.endmacro

.macro leave_tunix
    leave_data
    pop io23
.endmacro

    .segment "LIB"

;;;;;;;;;;;;;
;;; ZPLIB ;;;
;;;;;;;;;;;;;

; Init s, d and c with values at XA.
.export sset
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MEMORY BLOCK CLEAR/MOVE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Copy descriptor 'set' over copy
; vectors and move it, baby.
.macro smemcpyax set
    ldaxi set
    jsr smemcpy
.endmacro

; Copy range at XA.
.export smemcpy
.proc smemcpy
    jsr sset
    pushw s
    pushw d
    pushw c
    jsr memcpy
    popw c
    popw d
    popw s
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

l:  lda (s),y
    sta (d),y
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

; Copy memory (backwards).
; s: source
; d: destination
; c: count
.export memcpybw
.proc memcpybw
    ldy #0
    ldx c
    inx
    inc c+1
    bne copy_backwards

l2: lda (s),y
    sta (d),y
    dey
    cpy #$ff
    beq m2
copy_backwards:
q2: dex
    bne l2
    dec c+1
    bne l2
    rts
m2: dec s+1
    dec d+1
    jmp q2
.endproc


; Clear memory.
; d, c: dest, counter
.export bzero
.proc bzero
    ldx cl
    inx
    inc ch
    ldy dl
    lda #0
    sta dl
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

.macro error asciiz
    print asciiz
    jmp halt
.endmacro

.export print_cr
.proc print_cr
    phx
    lda #13
    jsr BSOUT
    plx
    rts
.endproc

.export print_comma
.proc print_comma
    phx
    lda #','
    jsr BSOUT
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
    lda #0
    jsr PRTFIX
    plx
    rts
.endproc

.export print_decword
.proc print_decword
    sta tmp1
    txa
    lda tmp1
    jmp PRTFIX
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
    lda tmp1
    jmp PRTFIX
.endproc

; Print '$' and hexadecimal byte in A.
.export print_hexbyte
.proc print_hexbyte
    pha
    lda #'$'
    jsr BSOUT
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
    lda #','
    jsr BSOUT
    lda tmp1
    jsr print_hexbyte
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
    lda #','
    jsr BSOUT
    lda tmp1
    jsr print_decbyte
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
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;
;;; EXTENDED MEMORY ;;;
;;;;;;;;;;;;;;;;;;;;;;;

; Allocate bank
; Returns:
;  Z: Out of memory.
;  X: Bank #
.export balloc
.proc balloc
    phy
    ;; Draw from global pool.
    lpopx banks, free_bank
    cpx #0
    beq :+  ; Oops…
    ;; Own it.
    dpushx lbanks, lbanksb, first_lbank
    inc bank_refs,x
:   ply
    txa
    rts
.endproc

.export free_lbank
.proc free_lbank
    drmy lbanks, lbanksb, first_lbank
    rts
.endproc

; Free bank
; X: Bank #
.export bfree
.proc bfree
    dec bank_refs,x
    bmi invalid_bank
    bne :+
    lpushx banks, free_bank
:   drmx lbanks, lbanksb, first_lbank
    clc
    rts
invalid_bank:
    inc bank_refs,x
    sec
    rts
.endproc

; Free all banks of current process.
.export bprocfree
.proc bprocfree
    ldx first_lbank
:   phx
    jsr bfree
    plx
    lloopx lbanks, :-
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

.export copy_blk3_to_blk5
.proc copy_blk3_to_blk5
    sta blk3
    stx blk5
    lda speedcopy_blk3_to_blk5
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

.macro save_internal_ram_to_blk5
    lda speedcopy_lowmem_to_blk5
    jsr next_speedcopy
    smemcpyax vec_screen_to_blk5
    smemcpyax vec_color_to_blk5
    smemcpyax vec_vic_to_blk5
.endmacro

.macro save_internal_ram_to_blk5_y
    get_procblk_y proc_io23, blk5
    save_internal_ram_to_blk5
.endmacro

.macro save_internal_ram_to_blk5_x
    get_procblk_x proc_io23, blk5
    save_internal_ram_to_blk5
.endmacro

    .segment "MACHDEPDATA"

vec_io23_to_blk5:
  .word $9800, $b800, $07f0

    .segment "MACHDEP"

; Fork banks and save stack.
;
; The child will never see this code as
; if it was returning from this function
; after schedule() called switch().
;
; Y: child process ID
; Returns Y unaffected.
.export fork_raw
.proc fork_raw
    ldx pid
    stx tmp1
    lda proc_data,x
    sta tmp2
    tsx
    stx tmp2+1

    push ram123
    push io23
    push blk2
    push blk3
    push blk5

    ;;; Make child's per-process banks.
    ;; Allocate IO23 bank globally only
    ; as the new RAM123 containing the
    ; local bank list is not there yet.
    lpopx banks, free_bank
    txa
    sta proc_io23,y
    inc bank_refs,x
    pha
    ; Copy parent's IO23 into child's.
    sta blk5
    smemcpyax vec_io23_to_blk5
    ; Copy lowmem, screen, color & VIC.
    save_internal_ram_to_blk5

    ;; Fork shadow RAM123.
    ; Allocate bank.
    lpopx banks, free_bank
    txa
    sta proc_data,y
    inc bank_refs,x
    ; Copy parent's into child's.
    lda tmp2 ; (Parent's shadow RAM123.)
    sta blk3
    stx blk5
    lda speedcopy_blk3_to_blk5
    jsr next_speedcopy
    stx ram123 ; Map in new.
    ; Register new RAM123 locally.
    dpushx lbanks, lbanksb, first_lbank
    ; Map in child's IO23.
    ; Register new IO23 locally.
    tax
    dpushx lbanks, lbanksb, first_lbank
    ; Update PID and stack pointer.
    pop io23 ; (Child's IO23.)
    sty pid
    mvb stack, tmp2+1

    ;; Copy remaining banks.
    ; Copies from BLK3 to BLK5 with
    ; speed code in BLK2.
    .macro forkblky procblk
        ldx tmp1 ; parent
        lda procblk,x
        sta blk3
        jsr balloc
        sta procblk,y
        sta blk5
        lda speedcopy_blk3_to_blk5
        sta blk2
        jsr $4000
    .endmacro
    forkblky proc_ram123
    forkblky proc_blk2
    forkblky proc_blk3
    forkblky proc_blk5
    ; Must come last to preserve proc
    ; data.
    forkblky proc_blk1

    pop blk5
    pop blk3
    pop blk2
    pop io23
    pop ram123
    rts
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
    lpushx lfns, first_lfn
    phx
    lpopx glfns, glfns
    inc glfn_refs,x
    txa
    tay
    plx
    tya
    sta lfn_glfn,x
:   rts
.endproc

;; Free all LFNs of the current process.
.export free_lfns
.proc free_lfns
    ldy first_lfn
    beq done
:   ldx lfn_glfn,y
    dec glfn_refs,x
    bne :+  ; (Still used.)
    lpushx glfns, glfns
:   lloopy lfns, :--
done:
    rts
.endproc

;;;;;;;;;;;;;;;;;
;;; PROCESSES ;;;
;;;;;;;;;;;;;;;;;
;
; Low 1K, screen, color and VIC are
; on the same bank as the IO23 area,
; which is reserved for TUNIX and
; drivers.  A shadow RAM123, only banked
; in for TUNIX, holds additional per-
; process data.

    .segment "KERNELDATA"

items_proc_list:
  .byte "ID", 0
  .byte "FLAGS", 0
  .byte "EXITCODE", 0
  .byte 0

items_proc_info:
  .byte "ID", 0
  .byte "MEMORY", 0
  .byte "LFNS", 0
  .byte "NBANKS", 0
  .byte 0

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
    lda exit_codes,x
    jsr print_decbyte
    plx

;    phx
;    jsr print_comma
;    enter_data_x
;    ldaxi waiting
;    jsry list_length, first_wait
;    stx tmp1
;    leave_data
;    lda tmp1
;    jsr print_decbyte
;    plx

    jsr print_cr
    rts
.endproc

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
    jsr print_cr
    ldx #0
    jsr proc_list_item
    ldx running
    beq :+
l:  jsr proc_list_item
    lloopx procs, l
:   popw zp2
    clc
    rts
.endproc

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
    lda proc_data,x
    jsr print_chb
    lda proc_io23,x
    jsr print_chb
    lda proc_ram123,x
    jsr print_chb
    lda proc_blk1,x
    jsr print_chb
    lda proc_blk2,x
    jsr print_chb
    lda proc_blk3,x
    jsr print_chb
    lda proc_blk5,x
    jsr print_chb
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

; Fork current process.
; Returns:
; A: New process ID for the parent and
;    0 for the child.
.export fork
.proc fork
    ;; Grab process slot.
    alloc_proc_running_y
    cpy #0
    bne :+
    jmp no_more_procs

:   ldx pid
    phx
    jsr fork_raw
    ; Parent and child return here with
    ; the same stack contents but
    ; different PIDs on IO23, so we can
    ; tell them apart.
    plx ; Parent's PID.
    cpx pid
    bne child

    ;; Remove parent banks from child.
    enter_data_y
    ldx pid
    .macro unref_lbank_x procblk
        ldy procblk,x
        jsr free_lbank
    .endmacro
    phy
    unref_lbank_x proc_data
    unref_lbank_x proc_ram123
    unref_lbank_x proc_io23
    unref_lbank_x proc_blk1
    unref_lbank_x proc_blk2
    unref_lbank_x proc_blk3
    unref_lbank_x proc_blk5
    ply
    leave_data

    ;;; Increment child's GLFN refs.
    ;; Enter child's RAM123.
    enter_data_y
    ;; Increment GLFN of each LFN.
    ldx first_lfn
    beq :++
:   inc glfn_refs,x
    lloopx lfns, :-
    ;; Leave child's RAM123.
:   leave_data

    ;; Mark child as baby.
    lda #PROC_BABY
    sta proc_flags,y
    tya
    clc
    rts

child:
    get_procblk_y proc_data, ram123
    get_procblk_y proc_io23, io23
    get_procblk_y proc_blk2, blk2
    get_procblk_y proc_blk3, blk3
    get_procblk_y proc_blk5, blk5
    lda #0
    clc
    rts
.endproc

.export no_more_procs
.proc no_more_procs
    pop blk1
    sec
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
    lda #PROC_SLEEPING
    sta proc_flags,x
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
    lda #PROC_RUNNING
    sta proc_flags,x
    clc
    rts
not_to_resume:
    sec
    rts
.endproc

; Free all resources of a process and
; turn it into a zombie.
; X: Process ID.
.export zombify
.proc zombify
    lda proc_flags,x
    bne :+
    sec
    rts

    ;; Close LFNs and free banks.
:   phx
    enter_tunix_x
    jsr free_lfns
    jsr bprocfree
    leave_tunix
    plx

    phx
    jsr free_iopages
    jsr free_drivers
    plx

    ;; Remove process from running or
    ;; sleeping list.
    ldx tmp1
    lda proc_flags,x
    bmi take_off_sleeping
take_off_running:
    drmx procs, procsb, running
    jmp put_on_zombie_list
take_off_sleeping:
    drmx procs, procsb, sleeping

put_on_zombie_list:
    lpushx procs, zombie
    lda #PROC_ZOMBIE
    sta proc_flags,x
    clc
    rts
.endproc

; Resume waiting process
; X: ID of process waiting for
.export resume_waiting
.proc resume_waiting
    enter_tunix_x
    ldy first_wait
    beq done
    ldx waiting_pid,y
    leave_tunix
    jsr resume
    jmp schedule
done:
    leave_tunix
    jmp schedule
.endproc

; Wait for process to exit
; X: Process ID
; Returns:
; A: Exit code
.export wait
.proc wait
    lda proc_flags,x
    beq invalid_pid

    ; Put us on waiting list of the
    ; process.
    mvb tmp1, pid
    enter_tunix_x ; TODO: data only.
    alloc_waiting_y
    lda tmp1
    sta waiting_pid,y
    leave_tunix

check_if_zombie:
    lda proc_flags,x
    bne :+
    jmp guru_meditation
:   cmp #PROC_ZOMBIE
    beq end_wait

    ; Take a nap.
    phx
    phy
    push blk1
    ldx pid
    jsr suspend
    jsr schedule
    pop blk1
    ply
    plx
    jmp check_if_zombie

invalid_pid:
    sec
    rts
.endproc

; Stop process from waiting.
; X: Zombie process ID.
; Y: Slot in zombie's waiting list.
.export end_wait
.proc end_wait
    enter_tunix_x
    rm_waiting_y
    ldy first_wait
    beq reap_zombie

    ; Resume next waiting.
    phx
    tya
    tax
    jsr resume
    plx

return_code:
    leave_tunix
    lda exit_codes,x
    clc
    rts

reap_zombie:
    reap_zombie_x
    lda #0
    sta proc_flags,x
    beq return_code ; (jmp)
.endproc

; Exit current process
; A: Exit code
.export exit
.proc exit
    ldx pid
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
    jsr resume_waiting
    clc
    rts
invalid_pid:
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
    enter_data_x

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
    lda tmp1
    sta signal_type,y
    lda tmp2
    sta signal_payload,y
    jmp resume

ignore:
    leave_data
    clc
    rts

out_of_slots:
    phx
    jsr resume
    plx
    push blk1
    jsr schedule
    pop blk1
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
    lda #0
    sta pending_signal_types,y
    lda signal_handler_l,x
    sta sigjmp + 1
    lda signal_handler_h,x
    sta sigjmp + 2
:
.endmacro

.macro sigaction2
    php
    pha
    lda sigjmp + 2
    beq :+
    phx
    phy
sigjmp:
    jsr $1234
    lda #0
    sta sigjmp + 2
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
    lda #0
    sta signal_handler_h,x
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

; Free IO pages of a process.
; X: process ID
.proc free_iopages
    stx tmp1
    ldy first_iopage
    beq r
l:  lda iopage_pid,y
    cmp tmp1
    bne n
    tax
    free_iopage_x
n:  lloopy iopages, l
r:  rts
.endproc

; "DA"
.export tunix_alloc_io_page
.proc tunix_alloc_io_page
    lda free_iopage
    beq no_more
    alloc_iopage_x
    ldx pid
    sta iopage_pid,x
    txa
    clc
    adc #IOPAGE_BASE - 1
    jmp respond
no_more:
    jmp respond_error
.endproc

; "DCp"
.export tunix_commit_io_page
.proc tunix_commit_io_page
    ldy #0
l:  cpy pid
    beq next
    lda proc_flags,y
    beq next

    ;; Copy page.
    phy
    push blk5
    io23x_at_blk5
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
.export tunix_free_io_page
.proc tunix_free_io_page
    lda SA
    sec
    sbc #IOPAGE_BASE - 1
    tax
    lda iopage_pid,x
    beq not_there
    cmp pid
    bne not_there
    free_iopage_x
    jmp respond_ok
not_there:
    jmp respond_error
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DRIVER REGISTRATION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Free drivers of a process.
; X: process ID
free_drivers:
    stx tmp1
    ldy drvs
    beq r
l:  lda drv_pid,y
    cmp tmp1
    bne n
    tax
    lpushx drvs, drvs
    ; Set device to KERNAL.
    lda drv_dev,y
    tax
    lda #0  ; KERNAL
    sta dev_drv,x
n:  lloopy drvs, l
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
    lpopx drvs, drvs
    beq :+

    ;; Populate slot.
    lda pid
    sta drv_pid,x
    tya
    sta drv_dev,x
    lda zp1
    sta drv_vl,x
    lda zp1+1
    sta drv_vh,x

    ;; Assign to device.
    txa
    sta dev_drv,y

:   rts
.endproc

; "DR"
.export tunix_register
.proc tunix_register
    ldy SA
    lda filename+2
    ldx filename+3
    jsr register
    bcs :+
    txa
    jmp respond
:   jmp respond_error
.endproc


;;;;;;;;;;;;;;;;;
;;; PROCESS 0 ;;;
;;;;;;;;;;;;;;;;;

    .segment "KERNELDATA"

err_out_of_running_procs:
  .byte "OUT OF RUNNING PROCS. HALTING."
  .byte 13, 0

    .segment "KERNEL"

.export proc0
.proc proc0
    ldx #0
    jsr suspend
    jsr schedule
    lda running
    bne proc0
    lda #0
    sta multitasking
    print err_out_of_running_procs
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

;; Syscalls
filename:       .res 256
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
    lda #0
    sta STATUS
    lda DEV
    sta DFLTN
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
    sta respond
    ldx #1
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
    jmp respond_error
.endproc

; "MA"
.export tunix_balloc
.proc tunix_balloc
    jsr balloc
    txa
    beq :+
    jmp respond
:   jmp respond_error
.endproc

; "MFb"
syscall1 tunix_bfree, bfree, ldx

;; PROCESSES

; "P?"
.export tunix_procs
.proc tunix_procs
    lda FNLEN
    cmp #1
    bne :+
    lda pid
    jmp respond1
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
    beq tunix_stop
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
syscall1 tunix_stop, stop, ldx
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
    lda filename+2
:   jsr kill
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
ai: jmp tunix_alloc_io_page
co: jmp tunix_commit_io_page
fi: jmp tunix_free_io_page
.endproc

;;;;;;;;;;;;
;;; INIT ;;;
;;;;;;;;;;;;
;
; Booting the thing.

    .zeropage

banks_ok:       .res 1
banks_faulty:   .res 1
bnk:            .res 1
col:            .res 1

    .segment "KERNEL"

.export init_ultimem_banks
.proc init_ultimem_banks
    ;; Ensure RAM bank configuration.
    lda #$ff
    sta ulticfg1
    sta ulticfg2
    ldx #4
:   txa
    lsr
    sta $9ff0,x
    inx
    lda #0
    sta $9ff0,x
    inx
    cpx #16
    bne :-
    rts
.endproc

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
    lda bnk
    sta (zp1),y
    iny
    lda bnk
    sta (zp1),y
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
    lpushx banks, free_bank
    lda #'.'
    jsr printbnk

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
    lda #'!'
    jsr printbnk
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
    lda #13
    jsr BSOUT
    lda #0
    sta col
r:  rts
.endproc

    .segment "KERNELDATA"

txt_no_ultimem:
  .byte "NO ULTIMEM/VIC-MIDIFOUND."
  .byte 13, 0
txt_faulty_banks:
  .byte " FAULTY BANKS.", 13,0
txt_ram_ok:
  .byte 13, "RAM OK.", 13,0
txt_banks_free:
  .byte "K RAM FREE.", 0

vec_io_reloc:
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

    .segment "KERNEL"

.export boot
.proc boot
    jsr FRESTOR
    jsr init_ultimem_banks

    ;; Clear global data.
    stwi d, __GLOBALBSS_RUN__
    stwi c, __GLOBALBSS_SIZE__
    jsr bzero

    ;; Init local (per-process).
    ; Copy I/O handlers.
    smemcpyax vec_io_reloc
    ; Clear data areas.
    stwi d, __LOCALBSS_RUN__
    stwi c, __LOCALBSS_SIZE__ - __ULTIMEM_SIZE__
    jsr bzero
    stwi d, __LOCALBSS2_RUN__
    stwi c, __LOCALBSS2_SIZE__
    jsr bzero

    ;; Clear rest of BLK1.
    stwi d, __LOCALCODE_LOAD__
    stwi c, $4000 - __LOCALCODE_LOAD__
    jsr bzero

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
    ; Pointers to next elements.
:   txa
:   clc
    adc #1
    sta glfns,x
    sta lfns,x
    cpx #MAX_IOPAGES + 1
    bcs :+
    sta iopages,x
:   cpx #MAX_PROCS
    bcs :+
    sta procs,x
    sta waiting,x
:   cpx #MAX_DRVS + 1
    bcs :+
    sta drvs,x
:   inx
    bne @l
    ;; Finish up.
    mvb free_proc, #1
    mvb free_wait, #1
    mvb free_iopage, #1
    mvb glfns, #1
    mvb drvs, #1
    lda #0
    sta procs + MAX_PROCS - 1
    sta waiting + MAX_PROCS - 1
    sta drvs + MAX_DRVS
    sta iopages + MAX_IOPAGES

    ;;; Init machdep.
    jsr init_ultimem
    jsr gen_speedcodes
    jsr init_ultimem_banks

    ;;; Make init process 0.
    print txt_starting_multitasking
    inc multitasking
    ;; Make holograhic process to fork.
    ; Unlink from free list.
    lda #0
    sta procs ; (Running alone.)
    ; Fill in banks for process 0.
    ldx #2
    stx proc_data
    stx proc_ram123
    inx
    stx tunix_io23
    stx proc_io23
    inx
    stx tunix_blk1
    stx proc_blk1
    inx
    stx proc_blk2
    inx
    stx proc_blk3
    inx
    stx proc_blk5
    ;; Fork all banks.
    ldx #0
    jsr fork_raw
    ; Continue with forked banks.
    ; TODO: Unref RAM123 as proc 0 will
    ; use the shadow RAM123.
    mvb ram123, proc_data
    mvb io23, proc_io23
    mvb blk1, proc_blk1
    sta tunix_blk1
    mvb blk2, proc_blk2
    mvb blk3, proc_blk3
    mvb blk5, proc_blk5
    ;; Mark as running.
    lda #PROC_RUNNING
    sta proc_flags

    print txt_registering

    ;;; Init KERNAL vectors.
    smemcpyax vec_backup_kernal ; Save
    smemcpyax vec_tunix_kernal ; Replace

    ;;; Make devices default to KERNAL.
    mvb drv_vl, #<old_kernal_vectors
    mvb drv_vh, #>old_kernal_vectors

    ;;; Register TUNIX device.
    ldaxi tunix_driver
    ldy #TUNIX_DEVICE
    jmp register
.endproc

    .segment "KERNELDATA"

tunix_vectors:
.word open, close, chkin, ckout, clrcn
.word basin, bsout, stop, getin, clall
.word usrcmd, load, save, blkin, bkout

    .segment "KERNELDATA"

txt_tunix:
  ;.byte PETSCII_CLRSCR
  .byte "STARTING TUNIX.", 13, 0
txt_booting:
  .byte 13, "BOOTING.", 13, 0
txt_starting_multitasking:
  .byte "STARTING MULTITASKING.", 13, 0
txt_registering:
  .byte "STARTING SYSCALLS.", 13, 0
txt_init:
  .byte "STARTING INIT.", 13, 0

;;;;;;;;;;;;;;;
;;; LIBRARY ;;;
;;;;;;;;;;;;;;;

    .segment "KERNELDATA"

.export cmd_fork, cmd_exit, cmd_kill
.export cmd_wait, cmd_getpid
.export cmd_proc_info

cmd_fork:   .byte "PF"
cmd_exit:   .byte "PE"
cmd_kill:   .byte "PKc"
cmd_wait:   .byte "PW"
cmd_getpid: .byte "P"
cmd_proc_list:  .byte "PL"
cmd_proc_info:  .byte "PI"

    .segment "KERNEL"

.export lib_schedule
.proc lib_schedule
    lda #TUNIX_DEVICE
    tax
    jsr SETLFS
    lda #0
    jsr SETNAM
    jmp OPEN
.endproc

.export lib_getpid
.proc lib_getpid
    ;; Check if back in init.
    lda #TUNIX_DEVICE
    tax
    jsr SETLFS
    lda #1
    ldx #<cmd_getpid
    ldy #>cmd_getpid
    jsr SETNAM
    jmp OPEN
.endproc

.export lib_fork
.proc lib_fork
    ;; Fork and wait for child to exit.
    lda #TUNIX_DEVICE
    tax
    jsr SETLFS
    lda #2
    ldx #<cmd_fork
    ldy #>cmd_fork
    jsr SETNAM
    jmp OPEN
.endproc

; A: Exit code.
.export lib_exit
.proc lib_exit
    tay
    lda #TUNIX_DEVICE
    tax
    jsr SETLFS
    lda #2
    ldx #<cmd_exit
    ldy #>cmd_exit
    jsr SETNAM
    jmp OPEN
.endproc

; A: process ID
; X: exit code
.export lib_kill
.proc lib_kill
    stx cmd_kill+2
    tay
    lda #TUNIX_DEVICE
    tax
    jsr SETLFS
    lda #3
    ldx #<cmd_kill
    ldy #>cmd_kill
    jsr SETNAM
    jmp OPEN
.endproc

; A: process ID
.export lib_wait
.proc lib_wait
    tay
    lda #TUNIX_DEVICE
    tax
    jsr SETLFS
    lda #2
    ldx #<cmd_wait
    ldy #>cmd_wait
    jsr SETNAM
    jmp OPEN
.endproc

; Print process info.
; A: process ID
.export lib_proc_list
.proc lib_proc_list
    lda #TUNIX_DEVICE
    tax
    jsr SETLFS
    lda #2
    ldx #<cmd_proc_list
    ldy #>cmd_proc_list
    jsr SETNAM
    jmp OPEN
.endproc

; Print process info.
; A: process ID
.export lib_proc_info
.proc lib_proc_info
    tay
    lda #TUNIX_DEVICE
    tax
    jsr SETLFS
    lda #2
    ldx #<cmd_proc_info
    ldy #>cmd_proc_info
    jsr SETNAM
    jmp OPEN
.endproc

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
:   lda proc_blk1,x
    jmp call_driver1
.endproc

.export open2
.proc open2
    stwi FNADR, filename
    push LFN
    jsr lfn_to_glfn
    sta LFN
    ldy DEV
    lda dev_drv,y
    tay
    jsra call_driver, #IDX_OPEN
    pop LFN
    popw FNADR
    lda reg_a
    jmp tunix_leave
.endproc

.export chkin2
.proc chkin2
    sta reg_a
    tax
    lda lfn_glfn,x
    beq :+
    tax
    lda glfn_drv,x
    tay
    lda drv_dev,y
    sta DFLTN
    clc
    jmp tunix_leave
:   sec
    jmp tunix_leave
.endproc

.export ckout2
.proc ckout2
    sta reg_a
    tax
    lda lfn_glfn,x
    beq :+
    tax
    lda glfn_drv,x
    tay
    lda drv_dev,y
    sta DFLTO
    clc
    jmp tunix_leave
:   sec
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
    ldy glfn_drv,x
    lda #0
    sta glfn_drv,x
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
    lloopx lfns, :-
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Static KERNAL I/O handlers. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    .segment "LOCALCODE"

; Schedule task switch
; Picks next or first on running list.
.export schedule
.proc schedule
    lda multitasking
    beq r

    ;; Get next running.
    ldx pid
    lda procs,x
    bne :++
    ; Restart list.
    lda proc_flags  ; (process 0)
    cmp #PROC_RUNNING
    bne :+  ; No...
    lda #0
    beq :++ ; (jmp)
:   lda running
    ; Avoid switch to self.
:   cmp pid
    beq r

    tay
    push ram123
    push blk2
    push blk3
    push blk5
    jsr switch ; Switch IO23 & lowmem.
    pop blk5
    pop blk3
    pop blk2
    pop ram123

r:  rts
.endproc

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
    sei

    ;;; Save current.
    tsx
    cpx #STACK_LIMIT
    bcs :+
    jsr stack_overflow
    ; NOT REACHED.
:   stx stack
    ldx io23
    stx blk5
    ldx pid
    save_internal_ram_to_blk5_x

    ;;; Load next.
    get_procblk_y proc_io23, blk5
    ;; ...color, screen and VIC config.
    smemcpyax vec_blk5_to_color
    smemcpyax vec_blk5_to_screen
    smemcpyax vec_blk5_to_vic
    ;; Copy in low mem...
    lda speedcopy_blk5_to_lowmem
    sta blk2
    ; Set return address of speed copy
    ; manually as we cannot use the
    ; stack since its just about to be
    ; overwritten.
    lda #<:+
    sta $5801
    lda #>:+
    sta $5802
    jmp $4000

    ;; Hop over.
:   mvb io23, blk5
    ldx stack
    txs
    cli
    rts
.endproc

.export call_driver1
.proc call_driver1
    sta blk1
.endproc

.export call_driver2
.proc call_driver2
    load_regs
.endproc

.export call_driver3
.proc call_driver3
    jsr $fffe
    save_regs
    php
    pla
    sta flags
    rts
.endproc

; Restore banks on BLK1 & RAM123.
.export tunix_leave
.proc tunix_leave
    jsr schedule

    ldx pid
    ldy proc_flags,x
    pop ram123
    pop blk1

    ; Init baby banks after fork().
    cpy #PROC_BABY
    bne :+
    mvb blk1,tunix_blk1
    lda #PROC_RUNNING
    sta proc_flags,x
    lda proc_ram123,x
    sta ram123
    lda proc_blk1,x
    sta blk1

    ; Restore syscall return values.
:   lda flags
    pha
    load_regs
    plp
    rts
.endproc

.export tunix_enter
.proc tunix_enter
    save_regs
.endproc

; Map in RAM123 and BLK1.
.export tunix_enter2
.proc tunix_enter2
    popw fnord
    push blk1
    push ram123
    pushw fnord
    mvb blk1, tunix_blk1
    ldx pid
    lda proc_data,x
    sta ram123
    rts
.endproc

fnord: .res 2

.export open
.proc open
    save_regs

    ;; Move filename + pointer to IO23.
    ldy FNLEN
    beq :++
    dey
:   lda (FNADR),y
    sta filename,y
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

.export clrcn
.proc clrcn
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

.ifdef EARLY_TESTS

;;;;;;;;;;;;;
;;; TESTS ;;;
;;;;;;;;;;;;;
;
; Tests running before regular boot.

    .segment "BOOT"

txt_tests:
  .byte "!!! RUNNING TESTS !!!", 13, 0
txt_testing_data:
  .byte "!!! TESTING DATA !!!", 13, 0
txt_testing_processes:
  .byte "!!! TESTING PROCS !!!", 13, 0
txt_child:
  .byte "BABY.", 13, 0
txt_hyperactive_child:
  .byte ":):):):):):):):):):):):)", 0
txt_tests_passed:
  .byte "!!!    SUCCESS:   !!!", 13
  .byte "!!! CHECKS PASSED !!!", 13, 0

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
err_wrong_free_proc_count:
  .byte "WRONG # OF FREE PROCS.", 0
err_cannot_fork:
  .byte "CANNOT FORK.", 0
err_child_running_after_exit:
  .byte "CHILD STILL RUNNING AFTER "
  .byte "EXIT.", 0
err_init_pid_not_0:
  .byte "INIT PID NOT 0 AFTER FORK.", 0
err_cannot_kill:
  .byte "CANNOT KILL.", 0
err_cannot_wait:
  .byte "CANNOT WAIT.", 0

    .segment "BOOT"

; TODO: Make a proper formula from
; defined constants.
FREE_BANKS_AFTER_INIT = MAX_BANKS - FIRST_BANK - 6 - 8 - 3

.export tests
.proc tests
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
:   lpopx procs, free_proc
    phx
    ldaxi procs
    jsry list_length, free_proc
    cpx #MAX_PROCS - 2 ; (+ init)
    beq :+
    error err_wrong_free_proc_count
:   plx
    ; Push free proc onto running.
    dpushx procs, procsb, running
    ldaxi procs
    jsry list_length, running
    cpx #1
    beq :+
    error err_wrong_deque_index
    ; Move it back to free procs.
:   drmx procs, procsb, running
    dpushx procs, procsb, free_proc

    ;;;;;;;;;;;;;;;;
    ;;; Syscalls ;;;
    ;;;;;;;;;;;;;;;;

    print txt_testing_processes

    ;; Fork and wait for child to exit.
    print note_forking
    jsr lib_fork
    bcc :+
    error err_cannot_fork
:   pha
    cmp #0
    bne :+
    jmp baby
:   jsr lib_proc_list
    lda #0
    jsr lib_proc_info
    pla
    pha
    jsr lib_proc_info
    ; Wait for child to exit.
    print note_waiting_for_child
    pla
    jsr lib_wait
    print note_child_exited

    ;; Check our process ID.
;    print note_getting_pid
;    jsr lib_getpid
;    cmp #0
;    beq :+
;    error err_init_pid_not_0

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
:   print txt_tests_passed
    rts
.endproc

.export baby
.proc baby
    print txt_child
    jsr lib_exit
    error err_child_running_after_exit
.endproc

.export hyperactive_child
.proc hyperactive_child
:   print txt_hyperactive_child
    jmp :-
.endproc

.endif ; .ifdef EARLY_TESTS

;;;;;;;;;;;;;
;;; START ;;;
;;;;;;;;;;;;;

    .segment "KERNELDATA"

txt_welcome:
  .byte 13 ; PETSCII_CLRSCR
  .byte "TUNIX - ", 0

    .segment "KERNEL"

.export start
.proc start
    print txt_tunix

.ifdef EARLY_TESTS
    ; Relocate boot & tests.
    stwi s, __BOOT_LOAD__ + __BOOT_SIZE__ - 1
    stwi d, __BOOT_RUN__ + __BOOT_SIZE__ - 1
    stwi c, __BOOT_SIZE__
    jsr memcpybw
.endif ; .ifdef EARLY_TESTS

    jsr boot

.ifdef EARLY_TESTS
    print txt_tests
    jsr tests
.endif ; .ifdef EARLY_TESTS

    print txt_init
    jsr fork
    cmp #0
    beq :+
    jmp proc0
:   lda #0
    jsr lib_proc_info
    jsr lib_getpid
    jsr lib_proc_info

    ;; BASIC cold start.
    jsr INITVCTRS
    jsr INITBA
    lda TXTTAB
    ldy TXTTAB+1
    jsr RAMSPC
    ldayi txt_welcome
    jsr PRTSTR
    ldaxi banks
    jsry list_length, free_bank
    txa
    jsr print_free_ram_a
    jsr print_cr
    ldayi $e437
    jsr PRTSTR
    jsr $e412
    ldx #$f8
    txs
    jmp READY
.endproc
