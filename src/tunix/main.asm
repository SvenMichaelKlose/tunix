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

;__VIC20__ = 1
.include "cbm_kernal.inc"

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

;;; TUNIX

MAX_LFNS    = 256   ; Has to be.
MAX_PROCS   = 64
MAX_DRVS    = 16
MAX_DEVS    = 32
MAX_IOPAGES = 4

;;; MACHDEP

IOPAGE_BASE = $7b

;;; UltiMem

.export ram123, io23, blk1, blk2, blk3, blk5

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

.export global_start, banks, free_bank, first_bank, bank_refs, iopages, iopagesb, free_iopage, first_iopage, iopage_pid, iopage_page, glfns, glfn_refs, glfn_drv, glfn_sa, procs, procsb, free_proc, running, sleeping, zombie, proc_flags, exit_codes, proc_low, proc_ram123, proc_io23, proc_blk1, proc_blk2, proc_blk3, proc_blk5, drvs, drv_pid, drv_dev, drv_vl, drv_vh, dev_drv, copy_bank, global_end, global_size, global_start

global_start:

;; Extended memory banks
banks:          .res MAX_BANKS
free_bank:      .res 1
first_bank:     .res 1
bank_refs:      .res MAX_BANKS

;; IO pages
iopages:        .res MAX_IOPAGES
iopagesb:       .res MAX_IOPAGES
free_iopage:    .res 1
first_iopage:   .res 1
iopage_pid:     .res MAX_IOPAGES
iopage_page:    .res MAX_IOPAGES

;; Global logical file numbers
;; Shared by fork()ed processes.
; Free list
glfns:      .res MAX_LFNS
glfn_refs:  .res MAX_LFNS
; Last parameters to OPEN.
glfn_drv:   .res MAX_LFNS
glfn_sa:    .res MAX_LFNS

;; Processes
procs:      .res MAX_PROCS
procsb:     .res MAX_PROCS
free_proc:  .res 1
running:    .res 1
sleeping:   .res 1
zombie:     .res 1
PROC_ZOMBIE     = 32
PROC_RUNNING    = 64
PROC_SLEEPING   = 128
proc_flags: .res MAX_PROCS
exit_codes: .res MAX_PROCS
; Current banks.
proc_low:   .res MAX_PROCS
proc_ram123:.res MAX_PROCS
proc_io23:  .res MAX_PROCS
proc_blk1:  .res MAX_PROCS
proc_blk2:  .res MAX_PROCS
proc_blk3:  .res MAX_PROCS
proc_blk5:  .res MAX_PROCS

;; Drivers
drvs:       .res MAX_DRVS
drv_pid:    .res MAX_DRVS
drv_dev:    .res MAX_DRVS
drv_vl:     .res MAX_DRVS
drv_vh:     .res MAX_DRVS

; Drivers assigned to devices.
dev_drv:    .res MAX_DEVS

;; First speed code BLK5 to copy from
;; BLK2 to BLK3.
copy_bank:  .res 1

global_end:
global_size = global_end - global_start

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

.macro linit list
    mvb list, #1
.endmacro

.macro lpopx list, free
    ldx free
    lda list,x
    sta free
    lda #0
    sta list,x
.endmacro

.macro lpopy list, free
    ldy free
    lda list,y
    sta free
    lda #0
    sta list,y
.endmacro

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

.macro lmovex list, from, to
    lpopy list, from
    lpushx list, to
.endmacro

.macro lmovey list, from, to
    lpopy list, from
    lpushy list, to
.endmacro

.macro lnextx list, loop
    lda list,x
    tax
    bne loop
.endmacro

.macro lnexty list, loop
    lda list,y
    tay
    bne loop
.endmacro

;;;;;;;;;;;;;;;;;;;;
;;; DEQUE MACROS ;;;
;;;;;;;;;;;;;;;;;;;;

.macro daddx fw, bw, first
    lda first
    sta fw,x
    lda #0
    sta bw,x
    stx first
.endmacro

.macro _dlinkx fw, bw, first_free
    tay
    lda bw,x
    sta bw,y
    ; Link previous to next.
    tay
    lda fw,x
    sta fw,y
.endmacro

.macro _dlinky fw, bw, first_free
    tax
    lda bw,y
    sta bw,x
    ; Link previous to next.
    tax
    lda fw,y
    sta fw,x
.endmacro

.macro dpopx fw, bw, first_free
    ldx first_free
    lda fw,x
    sta first_free
    _dlinkx fw, bw, first_free
.endmacro

.macro dpopy fw, bw, first_free
    ldy first_free
    lda fw,y
    sta first_free
    _dlinky fw, bw, first_free
.endmacro

.macro drmx fw, bw, first_free
    lda fw,x
    cpx first_free
    bne :+
    sta first_free
:   _dlinkx fw, bw, first_free
.endmacro

.macro drmy fw, bw, first_free
    lda fw,y
    cpy first_free
    bne :+
    sta first_free
:   _dlinky fw, bw, first_free
.endmacro

.macro dallocx fw, bw, from, to
    dpopx fw, bw, from
    daddx fw, bw, to
.endmacro

.macro dfreex fw, bw, from, to
    drmx fw, bw, from
    daddx fw, bw, to
.endmacro

.macro dmovex fw, bw, from, to
    dfreex fw, bw, from, to
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCESS BANK MACROS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; UI

.macro print asciiz
    lda #<asciiz
    ldy #>asciiz
    jsr PRTSTR
.endmacro

.macro error asciiz
    print asciiz
    jmp halt
.endmacro

;;;;;;;;;;;;
;;; INIT ;;;
;;;;;;;;;;;;

.export main
.proc main
    print txt_tunix
    print txt_tests
    jsr tests
    print txt_booting
    jmp init
.endproc

.export tests
.proc tests
    jsr init

    ;;; Data structures
    ; Draw GLFNs until empty.
    ldy #1
:   lpopx glfns, glfns
    stx tmp2
    cpy tmp2
    beq :+
    error err_invalid_glfn_order
:   iny
    bne :--

    ;; Doubly used list arrays.
    ; Allocate free, put back as used.
    lpopx banks, free_bank
    cpx #$77
    beq :+
    error err_invalid_first_free_bank
:   lpushx banks, first_bank
    ldaxi banks
    jsry list_length, free_bank
    cpx #$69
    beq :+
    error err_fail
:   ldaxi banks
    jsry list_length, first_bank
    cpx #1
    beq :+
    error err_fail
    ; In reverse.
:   lmovex banks, first_bank, free_bank
    ldaxi banks
    jsry list_length, free_bank
    cpx #$6a
    beq :+
    error err_fail
:   ldaxi banks
    jsry list_length, first_bank
    cpx #0
    beq :+
    error err_fail
 
    ;; Deque
    ; Allocate first.
:   dpopx procs, procsb, free_proc
    phx
    ldaxi procs
    jsry list_length, free_proc
    cpx #MAX_PROCS - 2 ; (+ init)
    beq :+
    error err_fail
:   plx
    ; Free by index.
    daddx procs, procsb, running
    ldaxi procs
    jsry list_length, running
    cpx #1
    beq :+
    error err_fail
:

    ;;; Syscalls
    jsr init

    ;; Fork
    jsr fork
    cmp #1
    beq :++
    cmp #0
    beq :+
    error err_fail
:   lda #0
    jmp exit
:

.export stop2
stop2:
    jsr schedule
    lda #1
    jsr wait
    rts
.endproc

.export halt
.proc halt
    jmp halt
.endproc

    .rodata

txt_tunix:  .byte 147   ; Clear screen.
            .byte "TUNIX", 13, 0
txt_tests:  .byte "TESTS..",0
txt_booting:.byte 13, "BOOTING..",0
txt_init:   .byte ".", 0

err_invalid_glfn_order:
    .byte "INVALID GLFN ORDER", 0
err_invalid_first_free_bank:
    .byte "INVALID FIRST FREE BANK", 0
err_fail:
    .byte "TEST FAILED", 0

    .code

.export init
.proc init
    ;; All banks are R/W RAM.
    ;; Default order expected.
    lda #%11111111
    sta $9ff1
    sta $9ff2

    ;; Clear data.
    stzwi d, global_start
    stzwi c, global_size
    jsr bzero
    stzwi d, $9800
    stzwi c, $07f0
    jsr bzero

    ;; Set up lists and tables.
    ldx #0
@l: txa
    beq :++
    sec
    sbc #1
    cpx #MAX_PROCS
    bcs :+
    sta procsb,x
:   txa
:   clc
    adc #1
    sta glfns,x
    sta lfns,x
    cpx #MAX_IOPAGES
    bcs :+
    sta iopages,x
:   cpx #MAX_PROCS
    bcs :+
    sta procs,x
:   cpx #MAX_DRVS
    bcs :+
    sta drvs,x
:   inx
    bne @l

    mvb free_proc, #1
    mvb glfns, #1
    mvb drvs, #1
    lda #0
    sta procs + MAX_PROCS - 1
    sta waiting + MAX_PROCS - 1
    sta drvs + MAX_DRVS - 1
    sta iopages + MAX_IOPAGES - 1

    jsr init_ultimem
    jsr gen_speedcode

    ;; Point devices to KERNAL.
    mvb drv_vl, #$1a
    mvb drv_vh, #$03

    ;; Make init process.
    print txt_init
    mvb proc_low, ram123
    mvb proc_ram123, ram123
    mvb proc_io23, io23
    mvb proc_blk1, blk1
    mvb proc_blk2, blk2
    mvb proc_blk3, blk3
    mvb proc_blk5, blk5
    ldy #0
    jmp fork0 ; Fork into process 0.
.endproc

;;;;;;;;;;;;;;;;;;
;;; SPEED COPY ;;;
;;;;;;;;;;;;;;;;;;

.export outa
.proc outa
    ldy #0
    sta (d),y
    inc dl
    bne :+
    inc dh
:   rts
.endproc

.macro out val
    jsra outa, val
.endmacro

.macro outzw at
    jsra outa, at
    jsra outa, at+1
.endmacro

.export gen_speedcode
.proc gen_speedcode
    push blk5
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
    pop blk5
    rts
.endproc

.export copy_blk3_to_blk5
.proc copy_blk3_to_blk5
    stx blk3
    lda copy_bank
.endproc

.export next_copy_bank
.proc next_copy_bank
    sta blk2
    jmp $4000
.endproc

;;;;;;;;;;;;;;;;;
;;; PROCESSES ;;;
;;;;;;;;;;;;;;;;;
;
; Low 1K, screen, color and VIC are
; on the same bank as the IO23 area
; which is reserved for TUNIX and
; driver code.

.export fork
.proc fork
    ;; Grab process slot.
    lpopy procs, free_proc
    cpy #0
    beq no_more_procs
.endproc

.export fork0
.proc fork0
    ;; Insert after current process.
    ldx pid
    lda running,x
    sta running,y
    tya
    sta running,x
    pha

    jsr fork_raw

    ;; Increment bank refs.
    ldx first_lbank
    beq :++
:   inc bank_refs,x
    lnextx lbanks, :-

    ;; Increment GLFNs.
    ldx first_lfn
    beq :++
:   inc glfn_refs,x
    lnextx lfns, :-

:   push blk5
    io23y_at_blk5
    tsx
    stx stack+$2000
    pop blk5

    lda #PROC_RUNNING
    sta proc_flags,y

    ;; Return PID.
:   pla
    cmp pid
    bne :+
    lda #0  ; (for child)
:   clc
    rts
.endproc

.export no_more_procs
.proc no_more_procs
    sec
    rts
.endproc

    .rodata

set_lowmem: .word $0000, $b000, $0400
set_screen: .word $1000, $a000, $1000
set_color:  .word $9400, $b400, $0400
set_vic:
    .word $9000, saved_vic+$2000, $0010

set_blk5_to_lowmem:
    .word $b000, $0000, $0400
set_blk5_to_screen:
    .word $a000, $1000, $1000
set_blk5_to_color:
    .word $b400, $9400, $0400
set_blk5_to_vic:
    .word saved_vic+$2000, $9000, $0010

set_io23: .word $9800, $b800, $07f0

    .code

.macro smemcpyax set
    ldaxi set
    jsr smemcpy
.endmacro

.export save_state
.proc save_state
    get_procblk_y proc_low, blk5
    smemcpyax set_lowmem
    smemcpyax set_screen
    ldaxi set_color
    jmp smemcpy
.endproc

.export load_state
.proc load_state
    get_procblk_y proc_low, blk5
    smemcpyax set_blk5_to_lowmem
    smemcpyax set_blk5_to_vic
    smemcpyax set_blk5_to_color
    ldaxi set_blk5_to_screen
    jmp smemcpy
.endproc

.macro cpyblk procblk, blk
    jsr balloc
    sta procblk,y
    sta blk5
    ldx blk
    jsr copy_blk3_to_blk5
.endmacro

.macro rmlbankx procblk
    ldy procblk,x
    jsr free_lbank
.endmacro

; Copy banks to new process
; Y: process ID
.export fork_raw
.proc fork_raw
    push blk2
    push blk3
    push blk5

    jsr balloc
    sta proc_low,y
    jsr save_state

    jsr balloc
    sta proc_io23,y
    sta blk5
    ldaxi set_io23
    jsr smemcpy
    sty pid+$2000

    cpyblk proc_ram123, ram123
    cpyblk proc_blk1, blk1
    cpyblk proc_blk2, blk2
    cpyblk proc_blk3, blk3
    cpyblk proc_blk5, blk5

    cpy pid
    beq :+

    ;; Remove parent banks from child's.
    push io23
    io23y
    ldx pid
    rmlbankx proc_low
    rmlbankx proc_ram123
    rmlbankx proc_io23
    rmlbankx proc_blk1
    rmlbankx proc_blk2
    rmlbankx proc_blk3
    rmlbankx proc_blk5
    pop io23

:   pop blk5
    pop blk3
    pop blk2
    rts
.endproc

; Force exit.
; X: Process ID.
.export zombify
.proc zombify
    lda proc_flags,x
    beq :+
    sec
    rts

    ;; Close LFNs and free banks.
:   phx
    push io23
    io23x
    jsr free_lfns
    jsr bprocfree
    pop io23

    ;; Free IO pages
    plx
    stx tmp1
    ldy first_iopage
    beq :++
:   lda iopage_pid,y
    cmp tmp1
    bne :+
    tax
    dfreex iopages, iopagesb, first_iopage, free_iopage
:   lnexty iopages, :--

    ;; Free drivers.
    ldy drvs
    beq :++
:   lda drv_pid,y
    cmp tmp1
    bne :+
    tax
    lpushx drvs, drvs
    ; Set device to KERNAL.
    lda drv_dev,y
    tax
    lda #0  ; KERNAL
    sta dev_drv,x
:   lnexty drvs, :--

    ;; Free process
    ldx tmp1
    ; Take off running or sleeping.
    lda proc_flags,x
    bmi :+
    drmx procs, procsb, running
    jmp :++
:   drmx procs, procsb, sleeping
    ; Add to free.
:   lpushx procs, zombie
    lda #PROC_ZOMBIE
    sta proc_flags,x
    clc
    rts
.endproc

.export resume_waiting
.proc resume_waiting
    push io23
    io23x
    ldy first_wait
    beq done
    tax
    jsr resume
done:
    pop io23
    rts
.endproc

; Wait for process to exit.
; X: Process ID
; Returns: A: Exit code
.export wait
.proc wait
    lda proc_flags,x
    beq not_there
    cmp #PROC_ZOMBIE
    beq terminate_zombie

    ;; Put us on waiting list.
    push io23
    push pid
    io23x
    ;dallocy waiting, free_wait, first_wait
    pla
    sta waiting_pid,y
    pop io23
    jsr sleep
    jsr schedule
    jmp wait

not_there:
    sec
    rts

terminate_zombie:
    push io23
    io23y
    ;; Remove from waiting list.
    drmx waiting, first_wait, free_wait
    ldx first_wait
    bne :+
    ;; Remove zombie.
    dfreex procs, procsb, zombie, free_proc
    jmp :+
    ;; Resume next waiting.
:   jsr resume
:   pop io23
    lda exit_codes,x
    clc
    rts
.endproc

; Put process to sleep.
; A: Process ID
.export sleep
.proc sleep
    lda proc_flags,x
    beq not_there
    bmi already_sleeping
    dmovex procs, procsb, running, sleeping
not_there:
already_sleeping:
    sec
    rts
.endproc

; Wake up process.
; A: Process ID
.export resume
.proc resume
    lda proc_flags,x
    beq not_there
    bpl already_running
    dmovex procs, procsb, sleeping, running
not_there:
already_running:
    sec
    rts
.endproc

; A: Process ID
.export kill
.proc kill
    lda #255
    sta exit_codes,x
    phx
    jsr zombify
    plx
    jmp resume_waiting
.endproc

; A: Exit code
.export exit
.proc exit
    pha
    jsrx zombify, pid
    pla
    ldx pid
    jmp resume_waiting
.endproc

; A: Exit code
.export terminate
.proc terminate
    pha
    jsrx sleep, pid
    plx
    jmp resume_waiting
.endproc

; Switch to process.
; A: Process ID
.export switch
.proc switch
    ;;; Save current.
    pha
    tsx
    inx
    stx stack
    ldy pid
    set_procblk_y proc_low, ram123
    set_procblk_y proc_ram123, ram123
    set_procblk_y proc_io23, io23
    set_procblk_y proc_blk1, blk1
    set_procblk_y proc_blk2, blk2
    set_procblk_y proc_blk3, blk3
    set_procblk_y proc_blk5, blk5
    jsr save_state

    ;; Load next.
    ply
    jsr load_state
    get_procblk_y proc_low, ram123
    get_procblk_y proc_ram123, ram123
    get_procblk_y proc_io23, io23
    get_procblk_y proc_blk1, blk1
    get_procblk_y proc_blk2, blk2
    get_procblk_y proc_blk3, blk3
    get_procblk_y proc_blk5, blk5
    ldx stack
    txs
    rts
.endproc

; Schedule task switch
.export schedule
.proc schedule
    php
    pha
    phx
    phy
    ldx pid
    lda procs,x
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
.export execute
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
.export balloc
.proc balloc
    sty tmp1
    ;; Draw from global pool.
    lpopx banks, free_bank
    cpx #0
    beq :+  ; Oops…
    ;; Own it.
    daddx lbanks, lbanksb, first_lbank
    inc bank_refs,x
:   ldy tmp1
    txa
    rts
.endproc

.export free_lbank
.proc free_lbank
    drmy lbanks, lbanksb, first_lbank
    rts
.endproc

; Free bank
;
; Ingnores already free ones.
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
:   jsr bfree
    lnextx lbanks, :-
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOGICAL FILE NUMBERS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.export free_lfns
.proc free_lfns
    ldy first_lfn
    beq done
:   ldx lfn_glfn,y
    dec glfn_refs,x
    bne :+  ; (Still used.)
    lpushx glfns, glfns
:   lnexty lfns, :--
done:
    rts
.endproc

;;;;;;;;;;;;;;;
;;; DRIVERS ;;;
;;;;;;;;;;;;;;;

; XA: vectors
; Y: device
; Returns: X: driver ID or 0.
.export register
.proc register
    sta ptr1
    stx ptr1+1

    ;; Get slot.
    lpopx drvs, drvs
    beq :+

    ;; Populate slot.
    lda pid
    sta drv_pid,x
    tya
    sta drv_dev,x
    lda ptr1
    sta drv_vl,x
    lda ptr1+1
    sta drv_vh,x

    ;; Assign to device.
    tax
    sta dev_drv,y

:   rts
.endproc

;;;;;;;;;;;;;;;;;;
;;; LIST UTILS ;;;
;;;;;;;;;;;;;;;;;;

.export list_length
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
.export smemcpy
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

;;;;;;;;;;;;;;;;;;;;;;
;;; SYSCALL DRIVER ;;;
;;;;;;;;;;;;;;;;;;;;;;

.macro syscall1 name, fun, load
    .export name
    .proc name
        load filename+2
        jsr fun
        bcs respond_error
        bcc respond_ok  ; (jmp)
    .endproc
.endmacro

    .rodata

tunix_driver:
    .word tunix_open, tunix, tunix
    .word tunix_basin, tunix, tunix
    .word tunix, tunix, tunix, tunix
    .word tunix, tunix, tunix, tunix
    .word tunix

    .code

.export tunix
.proc tunix
    clc
    rts
.endproc

.export tunix_memory
.proc tunix_memory
    lda filename+1
    cmp #'A'
    beq tunix_balloc
    cmp #'F'
    beq tunix_bfree
    jmp respond_error
.endproc

.export tunix_balloc
.proc tunix_balloc
    jsr balloc
    txa
    beq respond_error
    jmp respond
.endproc

.export tunix_bfree
.proc tunix_bfree
    ldx filename+2
    jsr bfree
    bcs respond_error
    bcc respond_ok  ; (jmp)
.endproc

.export tunix_open
.proc tunix_open
    lda FNLEN
    beq respond_ok
    lda filename
    cmp #'P'
    beq tunix_procs
    cmp #'D'
    beq d
    bne respond_error ; (jmp)
d:  jmp tunix_drivers
.endproc

.export tunix_procs
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

syscall1 tunix_kill, kill, ldx
syscall1 tunix_wait, wait, ldx
syscall1 tunix_stop, stop, ldx
syscall1 tunix_resume, resume, ldx

.export tunix_fork
.proc tunix_fork
    jsr fork
    bcs respond_error
    bcc respond ; (jmp)
.endproc

syscall1 tunix_exit, exit, lda
syscall1 tunix_terminate, terminate, lda

.export respond_error
.proc respond_error
    ldx #0
    sta responsep
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

; A: Value to respond with error code 0.
.export respond
.proc respond
    sta response+1
    lda #0
    sta response
    sta responsep
    ldx #2
    bne respond_len ; (jmp)
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
    
.export tunix_register
.proc tunix_register
    ldy filename+2
    lda filename+3
    ldx filename+4
    jsr register
    bcs respond_error
    txa
    bcc respond ; (jmp)
.endproc

.export tunix_alloc_io_page
.proc tunix_alloc_io_page
    lda free_iopage
    beq respond_error
    dallocx iopages, iopagesb, free_iopage, first_iopage
    ldx pid
    sta iopage_pid,x
    txa
    clc
    adc #IOPAGE_BASE
    bcc respond ; (jmp)
.endproc

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
    lda filename+2
    clc
    adc #IOPAGE_BASE
    sta ptr1+1
    clc
    adc #2
    sta ptr2+1
    lda #0
    sta ptr1
    sta ptr2
    tay
:   lda (ptr1),y
    sta (ptr2),y
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

.export tunix_drivers
.proc tunix_drivers
    lda filename+1
    cmp #'R'
    beq reg
    cmp #'A'
    beq all
    cmp #'C'
    beq tunix_commit_io_page
    cmp #'F'
    beq tunix_free_io_page
    jmp respond_error

reg:jmp tunix_register
all:jmp tunix_alloc_io_page
.endproc

.export tunix_free_io_page
.proc tunix_free_io_page
    ldx filename+2
    lda iopage_pid,x
    beq not_there
    dfreex iopages, iopagesb, first_iopage, free_iopage
    jmp respond_ok
not_there:
    jmp respond_error
.endproc

;;;;;;;;;;;;;;;
;;; ULTIMEM ;;;
;;;;;;;;;;;;;;;

    .zeropage

bnk:            .res 2
num_errors:     .res 2
col:            .res 1


    .code

.export init_ultimem
.proc init_ultimem
    ; Unhide UltiMem registers
    lda $9f55
    lda $9faa
    lda $9f01
    lda $9ff3
    cmp #$11
    beq has_ultimem

    lda #<txt_no_ultimem
    ldy #>txt_no_ultimem
    jsr printstr
    jmp halt

has_ultimem:
    ;; Write banks.
    lda #FIRST_BANK
    sta bnk
    lda #0
    sta bnk+1

l2: lda #$00
    sta ptr1
    lda #$a0
    sta ptr1+1
    lda bnk
    sta $9ffe
    lda bnk+1
    sta $9fff

l:  ldy #0
    lda bnk
    sta (ptr1),y
    iny
    lda bnk+1
    sta (ptr1),y
    inc ptr1
    inc ptr1
    bne l
    inc ptr1+1
    lda ptr1+1
    cmp #$a1
    bne l

    inc bnk
    lda bnk
    cmp #$80
    bne l2

    ;; Read banks.
    lda #FIRST_BANK
    sta bnk
    lda #0
    sta bnk+1
    sta col

l4: lda #$00
    sta ptr1
    lda #$a0
    sta ptr1+1
    lda bnk
    sta $9ffe
    lda bnk+1
    sta $9fff

l3: ldy #0
    lda bnk
    cmp (ptr1),y
    bne uerror
    iny
    lda bnk+1
    cmp (ptr1),y
    bne uerror

    inc ptr1
    inc ptr1
    bne l3
    inc ptr1+1
    lda ptr1+1
    cmp #$a1
    bne l3

    ldx bnk
    lpushx banks, free_bank
;    lda #'.'
;    jsr printbnk

next_bank:
    inc bnk
    lda bnk
    cmp #$80
    bne l4

    lda num_errors
    bne has_errors
    lda #<txt_ram_ok
    ldy #>txt_ram_ok
    jsr printstr
    jmp done

has_errors:
    jsr printnum
    lda #<txt_faulty_banks
    ldy #>txt_faulty_banks
    jsr printstr

done:
    ldaxi banks
    jsry list_length, free_bank
    txa
    jsr printnum
    lda #<txt_banks_free
    ldy #>txt_banks_free
    jsr printstr

    rts

.proc printnum
    rts
.endproc

uerror:
    inc num_errors
    lda #'!'
    jsr printbnk
    jmp next_bank
.endproc

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

.proc printstr
    sta ptr2
    sty ptr2+1

l:  ldy #0
    lda (ptr2),y
    beq done
    jsr BSOUT
    inc ptr2
    bne l
    inc ptr2+1
    bne l   ; (jmp)

done:
    rts
.endproc

    .rodata

txt_no_ultimem:
    .byte "NO ULTIMEM/VIC-MIDIFOUND."
    .byte 13, 0
txt_faulty_banks:
    .byte " FAULTY BANKS.", 13,0
txt_ram_ok:
    .byte 13, "RAM OK.", 13,0
txt_banks_free:
    .byte " 8K BANKS FREE.", 13,0

    .code

;;;;;;;;;;;;;;;;;
;;; DISPATCH ;;;;
;;;;;;;;;;;;;;;;;

.byte "DIPATCH"

; Translate local to global LFN.
; X: LFN
.export lfn_to_glfn
.proc lfn_to_glfn
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

; X: GLFN
; A: vector offset
.export call_driver
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

    .rodata

tunix_vectors:
.word open, chkin, ckout, basin, bsout
.word getin, clrcn, close, clall, stop
.word usrcmd, load, save, blkin, bkout

    .code

.export open
.proc open
    pushw FNADR
    push LFN
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
    jsra call_driver, #IDX_OPEN
    sta reg_a

    pop LFN
    popw FNADR
    php
    lda reg_a
    plp
    jmp schedule
.endproc

.export chkin
.proc chkin
    jsr lfn_to_glfn
    sta reg_a
    jmpa call_driver, #IDX_CHKIN
.endproc

.export ckout
.proc ckout
    jsr lfn_to_glfn
    sta reg_a
    jmpa call_driver, #IDX_CKOUT
.endproc

.macro iohandler name, lfn, drvop
    .export name
    .proc name
        save_regs
        push lfn
        jsr lfn_to_glfn
        sta lfn

        jsra call_driver, #drvop
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

.export clrcn
.proc clrcn
    jsr lfn_to_glfn
    sta reg_a
    ldy glfn_drv,x
    tya
    tax
    jsra call_driver, #IDX_CLRCN
    jmp schedule
.endproc

.export close
.proc close
    jsr lfn_to_glfn
    sta reg_a
    ldy glfn_drv,x
    lda #0
    sta glfn_drv,x
    tya
    tax
    jsra call_driver, #IDX_CLOSE
    jmp schedule
.endproc

.export clall
.proc clall
    ldx first_lfn
    beq r
:   phx
    jsr close
    plx
    lnextx lfns, :-
r:  rts
.endproc

.export stop
.proc stop
    ldx #0
    jsra call_driver, #IDX_STOP
    jmp schedule
.endproc

.macro blkiohandler name, idx
    .export name
    .proc name
        save_regs
        ldy DEV
        ldx dev_drv,y
        jsra call_driver, #idx
        jmp schedule
    .endproc
.endmacro

blkiohandler load, IDX_LOAD
blkiohandler save, IDX_SAVE

.export usrcmd
.proc usrcmd
    ldx #0
    jmpa call_driver, #IDX_STOP
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOCAL (per process) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

    .bss
    .org $9800

.export lbanks, lbanksb, first_lbank, lfns, lfnsb, lfn_glfn, first_lfn, waiting, waiting_pid, free_wait, first_wait, pid, ppid, reg_a, reg_x, reg_y, stack, flags, saved_vic, filename, response, response_len, responsep

;; Extended memory banks
; List of used ones.
lbanks:     .res MAX_BANKS
lbanksb:    .res MAX_BANKS
first_lbank:.res 1

;; Logical file numbers
; Deque of used ones
lfns:       .res MAX_LFNS
lfnsb:      .res MAX_LFNS
; Translations to global LFNs
lfn_glfn:   .res MAX_LFNS
first_lfn:  .res 1

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

;; Syscalls
; File name copied from calling process.
filename:       .res 256
; TUNIX syscall device response
response:       .res 8
response_len:   .res 1
responsep:      .res 1
