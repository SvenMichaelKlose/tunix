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

STATUS      = $90
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
IDX_CLRCN  = 8
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
banks:     .res MAX_BANKS
; Number of processes that own a single
; bank.
bank_refs:  .res MAX_BANKS

;; Global logical file numbers
;; Shared by fork()ed processes.
; List of free GLFNs
glfns:      .res MAX_LFNS
; Driver used with GLFN @ OPEN.
glfn_drv:   .res MAX_LFNS
; Secondary address of GLFN @ OPEN.
glfn_sa:    .res MAX_LFNS

;; Processes
; Free slots
procsf:      .res MAX_PROCS
procsb:      .res MAX_PROCS
; Flags
PROC_ZOMBIE     = 0
PROC_RUNNING    = 1
PROC_SLEEPING   = 128
proc_flags: .res MAX_PROCS
exit_codes: .res MAX_PROCS
; Primary banks allocated.
proc_low:   .res MAX_PROCS
proc_blk1:  .res MAX_PROCS
proc_blk2:  .res MAX_PROCS
proc_blk3:  .res MAX_PROCS
proc_io23:  .res MAX_PROCS
proc_blk5:  .res MAX_PROCS

;; Drivers
; Free driver slots
drvs:       .res MAX_DRVS
; Processes registered
drv_pid:    .res MAX_DRVS
; Vector tables
drv_vl:     .res MAX_DRVS
drv_vh:     .res MAX_DRVS

; Drivers assigned to devices.
dev_drv:    .res MAX_DEVS

;; Bank allocation
free_bank:  .res 1
first_bank: .res 1

;; First speed code BLK5 to copy from
;; BLK2 to BLK3.
copy_bank:  .res 1

;; Pointers into array 'proc'.
free_proc:  .res 1
running:    .res 1
sleeping:   .res 1
zombie:     .res 1

    .code

;;;;;;;;;;;;;;
;;; MACROS ;;;
;;;;;;;;;;;;;;

.macro mvb to, from
    lda from
    sta to
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

.macro stzwi to, val
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

;;;;;;;;;;;;;;;;;;;;
;;; DEQUE MACROS ;;;
;;;;;;;;;;;;;;;;;;;;
; With insert/remove by index.
; !: First element must have indexes of
; value 0.

.macro deque_popx fw, first
    list_popx fw, first
.endmacro

.macro deque_rmx fw, bw, first
    lda fw,x
    ; Link next to previous.
    tay
    lda bw,x
    sta bw,y
    ; Link previous to next.
    tay
    lda fw,x
    sta fw,y
    ; Add to free.
    lda first
    sta fw,x
    lda #0
    sta bw,x
    stx first
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
    ; Draw until empty.
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
    stzwi d, $9800
    stzwi c, $07f0
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
    sta procsf,x
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
    sta procsf+MAX_PROCS-1
    sta drvs+MAX_DRVS-1

    ;; Save initial set of banks.
    mvb proc_low, ram123
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

.proc outa
    ldy #0
    sta (d),y
    inc dl
    bne :+
    inc dh
:   rts
.endproc

.macro out val
    lda val
    jsr outa
.endmacro

.macro outzw at
    lda at
    jsr outa
    lda at+1
    jsr outa
.endmacro

.proc gen_speedcode
    ; Grab a new bank for BLK5.
    jsr balloc
    sta copy_bank
    sta blk5
    ; Source/dest argument values.
    stzwi ptr1, $6000
    stzwi ptr2, $a000
    ; Total move count.
    stzwi c, $2100

next_bank:
    ; Per bank move count.
    stzwi tmp1, ($2000 / 6)
    ; Bank fill pointer.
    stzwi d, $a000

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
    jsr outa
    out #OP_JMP_ABS
    out #<next_copy_bank
    out #>next_copy_bank
    pop blk5
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
    list_popy procsf, free_proc
    beq no_more_procs

    ;; Insert after current process.
    ldx pid
    lda running,x
    sta running,y
    tya
    sta running,x
    pha

    lda #PROC_RUNNING
    sta proc_flags,y
    jsr fork_raw

    ;; Increment bank refs.
    ldx first_lbank
    beq :++
:   inc bank_refs,x
    lda lbanks,x
    tax
    bne :-

    ;; Return PID.
:   pla
    cmp pid
    bne :+
    lda #0  ; (for child)
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

.macro get_procblk_x proc, blk
    lda proc,y
    sta blk
.endmacro

.macro get_procblk_y proc, blk
    lda proc,y
    sta blk
.endmacro

.macro set_procblk_x proc, blk
    lda blk
    sta proc,x
.endmacro

.macro set_procblk_y proc, blk
    lda blk
    sta proc,y
.endmacro

.macro smemcpyax set
    ldaxi set
    jsr smemcpy
.endmacro

.proc save_state
    get_procblk_y proc_low, blk5
    smemcpyax set_lowmem
    smemcpyax set_screen
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
    get_procblk_y proc_low, blk3
    ldx stack-$2000
    txs
    smemcpyax set_blk5_to_lowmem
    smemcpyax set_blk5_to_vic
    smemcpyax set_blk5_to_color
    ldaxi set_blk5_to_screen
    jmp smemcpy
.endproc

.macro cpyblk proc, blk
    jsr balloc
    sta proc,y
    sta blk5
    ldx blk
    jsr copy_blk3_to_blk5
.endmacro

.macro sta_lbankx proc, blk
    ldy proc,x
    sta lbanks,y
    sty blk
.endmacro

; Copy process to new banks.
.proc fork_raw
    jsr balloc
    sta proc_low,y
    jsr save_state

    jsr balloc
    sta proc_io23,y
    sta blk5
    ldaxi set_io23
    jsr smemcpy
    sty pid+$2000

    cpyblk proc_blk1, blk1
    cpyblk proc_blk2, blk2
    cpyblk proc_blk3, blk3
    cpyblk proc_blk5, blk5

    ;; Release and restore parent banks.
    ldx pid
    lda #0
    sta_lbankx proc_blk2, blk2
    sta_lbankx proc_blk3, blk3
    sta_lbankx proc_blk5, blk5
    rts
.endproc

; Force exit.
; A: Process ID.
.proc proc_free
    tax
    lda proc_flags,x
    beq not_there
    phx

    ;; Close resources.
    ; Switch to context.
    push io23
    get_procblk_x proc_io23, io23
    ; Free.
    jsr clall
    jsr free_lfns
    jsr bprocfree
    ; Restore context.
    pop io23

    plx

    ;; Free process
    ; Take off running or sleeping.
    lda proc_flags,x
    bmi :+
    deque_rmx procsf, procsb, running
    jmp :++
:   deque_rmx procsf, procsb, sleeping
    ; Add to free.
:   list_pushx procsf, zombie
    lda #PROC_ZOMBIE
    sta proc_flags,x
    clc
    rts
not_there:
    sec
    rts
.endproc

; X: Process ID
.proc resume_waiting
    ; Switch to context.
    push io23
    get_procblk_x proc_io23, io23
    ldy waiting
    beq done
    ; Resume waiting.
:   phy
    tax
    jsr resume
    ply
    ; Next waiting...
    lda waiting,y
    tay
    bne :-

done:
    pop io23
    rts
.endproc

; Wait for process to exit.
; A: Process ID
; Returns: A: Exit code
.proc wait
    tax
    lda proc_flags,x
    beq not_there
    cmp #PROC_ZOMBIE
    beq terminate_zombie
    ;; Put us on waiting list.
    push io23
    push pid
    set_procblk_x proc_io23, io23
    list_popy waiting, free_wait
    list_pushy waiting, first_wait
    pla
    sta waiting_pid,y
    pop io23
    txa
    jsr sleep
    jsr schedule

terminate_zombie:
    ;; Remove from zombie list.
    ;deque_rmx procsf, procsb, zombie
    list_pushx procsf, free_proc
    lda exit_codes,x
    clc
    rts

not_there:
    sec
    rts
.endproc

; Put process to sleep.
; A: Process ID
.proc sleep
    tax
    lda proc_flags,x
    beq not_there
    bmi sleeping_already
    deque_rmx procsf, procsb, running
    ;deque_addx procsf, procsb, sleeping
not_there:
sleeping_already:
    sec
    rts
.endproc

; Wake up process.
; A: Process ID
.proc resume
    tax
    lda proc_flags,x
    beq not_there
    bpl running_already
    deque_rmx procsf, procsb, sleeping
    ;deque_addx procsf, procsb, running
not_there:
running_already:
    sec
    rts
.endproc

; A: Process ID
.proc kill
    tax
    lda #255
    sta exit_codes,x
    phx
    jsr proc_free
    plx
    jmp resume_waiting
.endproc

; A: Exit code
.proc exit
    pha
    lda pid
    jsr proc_free
    pla
    ldx pid
    jmp resume_waiting
.endproc

; A: Exit code
.proc terminate
    pha
    lda pid
    jsr sleep
    pla
    jmp resume_waiting
.endproc

; Switch to process.
; A: Process ID
.proc switch
    ;;; Save current.
    pha
    ldy pid
    set_procblk_y proc_low, ram123
    set_procblk_y proc_io23, io23
    set_procblk_y proc_blk1, blk1
    set_procblk_y proc_blk2, blk2
    set_procblk_y proc_blk3, blk3
    set_procblk_y proc_blk5, blk5
    jsr save_state
    tsx
    stx stack-$2000

    ;; Load next.
    pop pid
    tay
    jsr load_state
    get_procblk_y proc_low, ram123
    get_procblk_y proc_io23, io23
    get_procblk_y proc_blk1, blk1
    get_procblk_y proc_blk2, blk2
    get_procblk_y proc_blk3, blk3
    get_procblk_y proc_blk5, blk5
    ldx stack-$2000
    txs
    rts
.endproc

; Schedule task switch
.proc schedule
    php
    pha
    phx
    phy
    ldx pid
    lda procsf,x
    bne :+
    lda running
:   cmp pid
    bne switch
    ply
    plx
    pla
    plp
    rts
.endproc

; XA: Start address
.proc execute
    stax ptr1
    ldx #$ff
    txs
    jmp (ptr1)
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
    sty tmp1
    ;; Draw from global pool.
    list_popx banks, free_bank
    beq :+  ; Oops…
    ;; Own it.
    inc bank_refs,x
    inc lbanks,x
:   ldy tmp1
    txa
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
    phy
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
r:  ply
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

;;;;;;;;;;;;;;
;;; DRIVER ;;;
;;;;;;;;;;;;;;

tunix_driver:
.word tunix_open, tunix, tunix
.word tunix_basin, tunix, tunix, tunix
.word tunix, tunix, tunix, tunix, tunix
.word tunix, tunix, tunix

.proc tunix
    clc
    rts
.endproc

.proc tunix_open
    lda FNLEN
    beq respond_ok
    lda filename
    cmp #'P'
    beq tunix_procs
    bne respond_error ; (jmp)
.endproc

.proc tunix_procs
    lda filename+1
    cmp #'F'
    beq tunix_fork
    cmp #'K'
    beq tunix_kill
    cmp #'W'
    beq tunix_wait
    cmp #'S'
    beq tunix_stop
    cmp #'R'
    beq tunix_resume
    bne respond_error   ; (jmp)
.endproc

.proc tunix_fork
    jsr fork
    bcs respond_error
    bcc respond ; (jmp)
.endproc

.macro syscall1 name, fun
    .proc name
        lda filename+2
        jsr fun
        bcs respond_error
        bcc respond_ok  ; (jmp)
    .endproc
.endmacro

syscall1 tunix_kill, kill
syscall1 tunix_wait, wait
syscall1 tunix_stop, stop
syscall1 tunix_resume, resume
syscall1 tunix_exit, exit
syscall1 tunix_terminate, terminate

.proc respond_error
    ldx #0
    sta responsep
    inx
    stx response
    stx response_len
    sec
    rts
.endproc

.proc respond_ok
    ldx #0
    stx response
    stx responsep
    inx
.endproc

.proc respond_len
    stx response_len
    clc
    rts
.endproc

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
    
; A: Value to respond with error code 0.
.proc respond
    sta response+1
    lda #0
    sta response
    sta responsep
    ldx #2
    bne respond_len ; (jmp)
.endproc

;;;;;;;;;;;;;;;;;
;;; DISPATCH ;;;;
;;;;;;;;;;;;;;;;;

; Translate local to global LFN.
.proc lfn_to_glfn
    tax
    lda lfn_glfn,x
    bne :+  ; Use existing...
    list_pushx lfns, first_lfn
    beq :+
    popy_glfn
    tya
    sta lfn_glfn,x
:   sta glfn
    rts
.endproc

; X: GLFN
; A: vector offset
.proc call_driver
    ; (vector base + A).
    ldy glfn_drv,x
    clc
    adc drv_vl,y
    sta j+1
    lda drv_vh,y
    adc #0
    sta j+2

    push blk1
    ldx drv_pid,y
    get_procblk_x proc_blk1, blk1
    load_regs
j:  jsr $fffe
    pop blk1
    rts
.endproc

tunix_vectors:
.word open, chkin, ckout, basin, bsout
.word getin, clrcn, close, clall, stop
.word usrcmd, load, save, blkin, bkout

.proc open
    pushw FNADR
    push LFN
    tax
    jsr lfn_to_glfn
    sta LFN

    ;; Copy file name.
    ldy FNLEN
    beq :++
:   lda (FNADR),y
    sta filename,y
    dey
    jmp :-
:   stzwi FNADR, filename

    ;; Assign driver to GLFN.
    ldy DEV
    lda dev_drv,y
    sta glfn_drv,x

    ;; Assign secondary address to GLFN.
    pha
    lda SA
    sta glfn_sa,x
    pla

    tax
    lda #IDX_OPEN
    jsr call_driver
    sta reg_a

    pop LFN
    popw FNADR
    php
    lda reg_a
    plp
    jmp schedule
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
        save_regs
        push lfn
        jsr lfn_to_glfn
        sta lfn

        lda #drvop
        jsr call_driver
        sta reg_a

        pop lfn
        php
        lda reg_a
        plp
        jmp schedule
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
    jsr call_driver
    jmp schedule
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
    jsr call_driver
    jmp schedule
.endproc

.proc clall
    ldx first_lfn
    beq r
:   phx
    jsr close
    plx
    lda lfns,x
    tax
    bne :-
r:  rts
.endproc

.proc stop
    ldx #0
    lda #IDX_STOP
    jsr call_driver
    jmp schedule
.endproc

.macro blkiohandler name, idx
    .proc name
        save_regs
        ldy DEV
        ldx dev_drv,y
        lda #idx
        jsr call_driver
        jmp schedule
    .endproc
.endmacro

blkiohandler load, IDX_LOAD
blkiohandler save, IDX_SAVE

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
waiting:    .res MAX_PROCS
waiting_pid:.res MAX_PROCS
free_wait:  .res 1
first_wait: .res 1
pid:        .res 1
ppid:       .res 1
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

; TUNIX device response
response:       .res 8
response_len:   .res 1
responsep:      .res 1
