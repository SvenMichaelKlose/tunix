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

__VIC20__ = 1

;;; CPU

OP_LDA_IMM  = $a9
OP_LDA_ABS  = $ad
OP_STA_ABS  = $8d
OP_JMP_ABS  = $4c
OP_RTS      = $60

;;; UltiMem

.export ram123, io23, blk1, blk2, blk3, blk5
MAX_BANKS   = 128
FIRST_BANK  = 8
ulticfg1    = $9ff1
ulticfg2    = $9ff2
ram123      = $9ff4
io23        = $9ff6
blk1        = $9ff8
blk2        = $9ffa
blk3        = $9ffc
blk5        = $9ffe

;;; KERNAL

;.include "cbm_kernal.inc"

FRESTOR = $FD52

READST  = $ffb7
SETLFN  = $ffba
SETNAM  = $ffbd
OPEN    = $ffc0
CLOSE   = $ffc3
CHKIN   = $ffc6
CKOUT   = $ffc9
CLRCN   = $ffcc
BASIN   = $ffcf
BSOUT   = $ffd2
LOAD    = $ffd5
SAVE    = $ffd8
SETTIM  = $ffdb
RDTIM   = $ffde
STOP    = $ffe1
GETIN   = $ffe4
CLALL   = $ffe7

DFLTN   = $99
DFLTO   = $9a
FNLEN   = $b7
LFN     = $b8
SA      = $b9
DEV     = $ba
FNADR   = $bb

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

;;; TUNIX

TUNIX_DEVICE = 31
MAX_LFNS     = 256   ; Has to be.
MAX_PROCS    = 64
MAX_DRVS     = 16
MAX_DEVS     = 32
MAX_IOPAGES  = 4

;;; MACHDEP

IOPAGE_BASE = $7b

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

ptr1:   .res 2
ptr2:   .res 2
ptr3:   .res 2

    .data

;;;;;;;;;;;;;;
;;; GLOBAL ;;;
;;;;;;;;;;;;;;

.export global_start, banks, free_bank, bank_refs, iopages, iopagesb, free_iopage, first_iopage, iopage_pid, iopage_page, glfns, glfn_refs, glfn_drv, procs, procsb, free_proc, running, sleeping, zombie, proc_flags, exit_codes, proc_ram123, proc_io23, proc_blk1, proc_blk2, proc_blk3, proc_blk5, drvs, drv_pid, drv_dev, drv_vl, drv_vh, dev_drv, copy_bank, global_end, global_size, global_start, banks_ok, banks_faulty

global_start:

;; Extended memory banks
banks:          .res MAX_BANKS
free_bank:      .res 1
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

old_kernal_vectors:
            .res 32

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

.macro linit list
    mvb list, #1
.endmacro

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

.macro drmx fw, bw, first
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
:
.endmacro

.macro drmy fw, bw, first
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
:
.endmacro

.macro dallocx fw, bw, from, to
    lpopx fw, from
    dpushx fw, bw, to
.endmacro

.macro dallocy fw, bw, from, to
    lpopy fw, from
    dpushy fw, bw, to
.endmacro

.macro dmovex fw, bw, from, to
    drmx fw, bw, from
    dpushx fw, bw, to
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

.macro set_procblk_x proc, blk
    push blk1
    push blk
    mvb blk1, tunix_blk1
    pla
    sta proc,x
    pop blk1
.endmacro

.macro set_procblk_y proc, blk
    push blk1
    push blk
    mvb blk1, tunix_blk1
    pla
    sta proc,y
    pop blk1
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

;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCESS CONTEXT ;;;
;;;;;;;;;;;;;;;;;;;;;;;

.macro enter_context_x
    push io23
    io23x
.endmacro

.macro enter_context_y
    push io23
    io23y
.endmacro

.macro leave_context
    pop io23
.endmacro

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

;;;;;;;;;;;;;;
;;; STDLIB ;;;
;;;;;;;;;;;;;;

; Copy range at XA.
.export smemcpy
.proc smemcpy
    jsr sset
.endproc

; Copy memory.
; s, d, c: source, dest, counter
.export memcpy
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

;;;;;;;;;;
;;; UI ;;;
;;;;;;;;;;

.export printstr
.proc printstr
    mvb tmp1, #0
    jsr CLALL
    phx
:   ldy tmp1
    lda (ptr3),y
    beq :+
    jsr BSOUT
    inc tmp1
    bne :-  ; (jmp)
:   plx
    rts
.endproc

.macro print asciiz
    stwi ptr3, asciiz
    jsr printstr
.endmacro

.macro error asciiz
    print asciiz
    jmp halt
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;
;;; EXTENDED MEMORY ;;;
;;;;;;;;;;;;;;;;;;;;;;;
;
; Globally, only free banks are tracked
; via 'banks[]' and '*free_bank'.
; The latter could be stored in
; banks[0] instead. Items in 'bank_refs'
; increment for each process that shares
; a bank.
;
; Locally, only allocated banks are
; tracked in deque 'lbanks/lbanksb',
; starting with 'first_lbank'.

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
:   phx
    jsr bfree
    plx
    lnextx lbanks, :-
    rts
.endproc

;;;;;;;;;;;;;;;;;;
;;; SPEED COPY ;;;
;;;;;;;;;;;;;;;;;;

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

.export gen_speedcode
.proc gen_speedcode
    push blk5
    ; Grab a new bank for BLK5.
    jsr balloc
    sta copy_bank
    sta blk5
    ; Source/dest argument values.
    stwi ptr1, $6000
    stwi ptr2, $a000
    ; Total move count.
    stwi c, $2100

next_bank:
    ; Per bank move count.
    stwi tmp1, ($2000 / 6)
    ; Bank fill pointer.
    stwi d, $a000

next_move:
    ;; Make move.
    out #OP_LDA_ABS
    outw ptr1
    out #OP_STA_ABS
    outw ptr2

    ;; Step
    ; Increment argument values.
    incw ptr1
    incw ptr2
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
    sta blk3
    stx blk5
    lda copy_bank
.endproc

.export next_copy_bank
.proc next_copy_bank
    sta blk2
    jmp $4000
.endproc

;;;;;;;;;;;;;;;;;;;;;;
;;; MACHDEP VIC-20 :::
;;;;;;;;;;;;;;;;;;;;;;

    .rodata

vec_vic_to_blk5:
    .word $9000, saved_vic+$2000, $0010
vec_screen_to_blk5:
    .word $1000, $a000, $1000
vec_lowmem_to_blk5:
    .word $0000, $b000, $0400
vec_color_to_blk5:
    .word $9400, $b400, $0400

vec_blk5_to_screen:
    .word $a000, $1000, $1000
vec_blk5_to_lowmem:
    .word $b000, $0000, $0400
vec_blk5_to_color:
    .word $b400, $9400, $0400
vec_blk5_to_vic:
    .word saved_vic+$2000, $9000, $0010

    .code

.macro smemcpyax set
    ldaxi set
    jsr smemcpy
.endmacro

.macro save_internal_ram_to_blk5
    smemcpyax vec_screen_to_blk5
    smemcpyax vec_lowmem_to_blk5
    smemcpyax vec_color_to_blk5
    smemcpyax vec_vic_to_blk5
.endmacro

.macro save_internal_ram_to_blk5_y
    get_procblk_y proc_io23, blk5
    save_internal_ram_to_blk5
.endmacro

.macro load_internal_ram_from_blk5_y
    get_procblk_y proc_io23, blk5
    smemcpyax vec_blk5_to_lowmem
    smemcpyax vec_blk5_to_color
    smemcpyax vec_blk5_to_screen
    smemcpyax vec_blk5_to_vic
.endmacro

    .rodata

vec_io23_to_blk5:
    .word $9800, $b800, $07f0

    .code

; Copy banks to new process
; Y: new process ID
.export fork_raw
.proc fork_raw
    push io23
    mvb tmp1, blk1
    mvb tmp1+1, blk2
    mvb tmp2, blk3
    mvb tmp2+1, blk5
    mvb blk1, tunix_blk1

    ;; Make child's IO23 and use it.
    ; Allocate IO23 bank globally.
    lpopx banks, free_bank
    inc bank_refs,x
    txa
    sta proc_io23,y
    phx
    ; Copy IO23.
    sta blk5
    ldaxi vec_io23_to_blk5
    jsr smemcpy
    ; Low, screen, color, VIC.
    save_internal_ram_to_blk5
    pop io23
    sty pid  ; New PID.
    tax      ; Add local ref.
    ; Register new IO23 bank locally.
    dpushx lbanks, lbanksb, first_lbank
    ; Set return stack.
    tsx
    inx
    sta stack

    ;; Copy remaining banks.
    ; Copies from BLK3 to BLK5 with
    ; speed code in BLK2.
    .macro forkblky procblk, srcblk
        jsr balloc
        sta procblk,y
        lda srcblk
        jsr copy_blk3_to_blk5
    .endmacro
    forkblky proc_ram123, ram123
    forkblky proc_blk2, tmp1+1
    forkblky proc_blk3, tmp2
    forkblky proc_blk5, tmp2+1
    forkblky proc_blk1, tmp1

    cpy #0
    beq :+  ; Do not undo init proc.

    ;; Remove parents default banks.
    ldx pid
.macro dereflbankx procblk
    ldy procblk,x
    jsr free_lbank
.endmacro
    dereflbankx proc_ram123
    dereflbankx proc_io23
    dereflbankx proc_blk1
    dereflbankx proc_blk2
    dereflbankx proc_blk3
    dereflbankx proc_blk5

    ;; Restore bank set.
:   mvb blk5, tmp2+1
    mvb blk3, tmp2
    mvb blk2, tmp1+1
    mvb blk1, tmp1
    pop io23
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

;;;;;;;;;;;;;;;;;
;;; PROCESSES ;;;
;;;;;;;;;;;;;;;;;
;
; Low 1K, screen, color and VIC are
; on the same bank as the IO23 area
; which is reserved for TUNIX and
; driver code.

.macro enter_tunix
.endmacro

.export fork
.proc fork
    ;; Grab process slot.
    dallocy procs, procsb, free_proc, running
    cpy #0
    beq no_more_procs
.endproc

; Y: New process ID.
.export fork0
.proc fork0
    ;; Increment bank refs.
    ldx first_lbank
    beq :++
:   inc bank_refs,x
    lnextx lbanks, :-

    ;; Increment GLFNs.
:   ldx first_lfn
    beq :++
:   inc glfn_refs,x
    lnextx lfns, :-

    ;; Machine-dependend process copy.
:   phy
    jsr fork_raw
    ply

    lda #PROC_RUNNING
    sta proc_flags,y

    ;; Return PID.
    tya
    cmp pid
    bne :+
    lda #0  ; (for child)
:   clc
    rts
.endproc

.export no_more_procs
.proc no_more_procs
    pop blk1
    sec
    rts
.endproc

; Put process to sleep.
; X: Process ID
.export sleep
.proc sleep
    lda proc_flags,x
    beq not_there
    bmi already_sleeping
    dmovex procs, procsb, running, sleeping
    clc
    rts
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
    bpl not_to_resume
    dmovex procs, procsb, sleeping, running
not_to_resume:
    sec
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
    enter_context_x
    jsr free_lfns
    jsr bprocfree
    leave_context
    plx

    ;; Free IO pages.
    stx tmp1
    ldy first_iopage
    beq :+++
:   lda iopage_pid,y
    cmp tmp1
    bne :+
    tax
    dmovex iopages, iopagesb, first_iopage, free_iopage
:   lnexty iopages, :--

    ;; Free drivers.
:   ldy drvs
    beq :+++
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

    ;; Free process.
:   ldx tmp1
    ; Take off running or sleeping.
    lda proc_flags,x
    bmi :+
    drmx procs, procsb, running
    jmp :++
:   drmx procs, procsb, sleeping
    ; Put on zombie list.
:   lpushx procs, zombie
    lda #PROC_ZOMBIE
    sta proc_flags,x
    clc
    rts
.endproc

; Resume waiting processes
; X: ID of process waiting for
.export resume_waiting
.proc resume_waiting
    enter_context_x
    ldy first_wait
    beq done
    jsr resume
done:
    leave_context
    ldx running
    lda procs,x
:   beq :-  ; Idle for interrupts.
    jmp schedule
.endproc

; Wait for process to exit
; X: Process ID
; Returns: A: Exit code
.export wait
.proc wait
    lda proc_flags,x
    beq not_there

    ;; Put us on waiting list.
    enter_context_x
    dallocy waiting, waitingb, free_wait, first_wait
    txa
    sta waiting_pid,y
    leave_context

:   lda proc_flags,x
    cmp #PROC_ZOMBIE
    beq terminate_zombie

    ;; Take a nap.
    phx
    lda pid
    jsr sleep
    jsr schedule
    plx
    jmp :-

not_there:
    sec
    rts

terminate_zombie:
    enter_context_y
    ;; Remove from waiting list.
    dmovex waiting, waitingb, first_wait,free_wait
    ldx first_wait
    bne :+
    ;; Remove zombie.
    dmovex procs, procsb, zombie, free_proc
    jmp :++

    ;; Resume next waiting.
:   jsr resume
:   leave_context
    lda exit_codes,x
    clc
    rts
.endproc

; Kill process with exit code -1
; X: Process ID
.export kill
.proc kill
    lda #255
    sta exit_codes,x
    jsr zombify
    jmp resume_waiting
.endproc

; Exit current process
; A: Exit code
.export exit
.proc exit
    ldx pid
    sta exit_codes,x
    jsr zombify
    jmp resume_waiting
.endproc

; XA: Start address
.export execute
.proc execute
    stax ptr1
    ldx #$ff
    txs
    jmp (ptr1)
.endproc

;;;;;;;;;;;;;;;
;;; DRIVERS ;;;
;;;;;;;;;;;;;;;

; Register driver and assign to device
; XA: vectors
; Y: device
; Returns: X: driver ID or 0.
.export register
.proc register
    stax ptr1

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
    txa
    sta dev_drv,y

:   rts
.endproc

;;;;;;;;;;;;;;
;;; DRIVER ;;;
;;;;;;;;;;;;;;

    .rodata

.export tunix_driver
tunix_driver:
    .word tunix_open, tunix, tunix
    .word tunix, tunix, tunix_basin
    .word tunix, tunix, tunix, tunix
    .word tunix, tunix, tunix, tunix
    .word tunix

    .code

.macro syscall1 name, fun, load
    .export name
    .proc name
        load filename+2
        jsr fun
        bcs respond_error
        bcc respond_ok  ; (jmp)
    .endproc
.endmacro

.export tunix
.proc tunix
    clc
    rts
.endproc

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
.export tunix_bfree
.proc tunix_bfree
    ldx filename+2
    jsr bfree
    bcs :+
    jmp respond_ok
:   jmp respond_error
.endproc

.export tunix_open
.proc tunix_open
    mvb DFLTN, #TUNIX_DEVICE
    lda FNLEN
    beq s
    lda filename
    cmp #'M'
    beq tunix_memory
    cmp #'P'
    beq tunix_procs
    cmp #'D'
    beq d
    bne respond_error ; (jmp)
d:  jmp tunix_drivers
s:  jmp schedule
.endproc

; "P?"
.export tunix_procs
.proc tunix_procs
    lda filename+1
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
    bne respond_error   ; (jmp)
.endproc

syscall1 tunix_kill, kill, ldx
syscall1 tunix_wait, wait, ldx
syscall1 tunix_stop, stop, ldx
syscall1 tunix_resume, resume, ldx
syscall1 tunix_exit, exit, lda

; "PF"
.export tunix_fork
.proc tunix_fork
    jsr fork
    bcs respond_error
    bcc respond ; (jmp)
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

; Respond with value and error code 0.
; A: value
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

; "DR"
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

; "DA"
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
    lda filename+2
    clc
    adc #IOPAGE_BASE
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

; "D?"
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

; "DFp"
.export tunix_free_io_page
.proc tunix_free_io_page
    lda filename+2
    sec
    sbc #IOPAGE_BASE
    tax
    lda iopage_pid,x
    beq not_there
    cmp pid
    bne not_there
    dmovex iopages, iopagesb, first_iopage, free_iopage
    jmp respond_ok
not_there:
    jmp respond_error
.endproc

;;;;;;;;;;;;
;;; INIT ;;;
;;;;;;;;;;;;

    .zeropage

banks_ok:       .res 1
banks_faulty:   .res 1
bnk:            .res 1
col:            .res 1

    .code

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
    stwi ptr1, $a000
    mvb blk5, bnk

write_byte:
    ldy #0
    lda bnk
    sta (ptr1),y
    iny
    lda bnk
    sta (ptr1),y
    inc ptr1
    inc ptr1
    bne write_byte
    inc ptr1+1
    lda ptr1+1
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
    stwi ptr1, $a000
    mvb blk5, bnk

read_byte:
    ldy #0
    lda bnk
    cmp (ptr1),y
    bne uerror  ; Eek!
    iny
    lda bnk
    cmp (ptr1),y
    bne uerror  ; Eek!

    inc ptr1
    inc ptr1
    bne read_byte
    inc ptr1+1
    lda ptr1+1
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
    jmp done

uerror:
    inc banks_faulty
    lda #'!'
    jsr printbnk
    jmp next_bank
.endproc

.export bank_last
bank_last = banks + $7f

has_errors:
    jsr printnum
    print txt_faulty_banks

done:
    ldaxi banks
    jsry list_length, free_bank
    cpx banks_ok
    beq :+
    error err_ultimem_num_banks_in_list
:   txa
    jsr printnum
    print txt_banks_free
    rts

err_ultimem_num_banks_in_list:
    .byte "WRONG NUMBER OF ITEMS IN "
    .byte "'BANKS'.", 0

.export printnum
.proc printnum
    rts
.endproc

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

io_size = io_end - io_start

.export init
.proc init
    jsr FRESTOR
    jsr init_ultimem_banks

    ;; Clear global data.
    stwi d, global_start
    stwi c, global_size
    jsr bzero

    ;; Init local data.
    ; Clear.
    stwi d, io_end
    stwi c, $07f0-io_size
    jsr bzero
    ; Move in code.
    stwi s, io_load
    stwi d, $9800
    stwi c, io_end-io_start
    jsr memcpy

    ;; Link lists.
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

    ;; Init machdep.
    jsr init_ultimem
    jsr gen_speedcode
    jsr init_ultimem_banks

    ;; Make init process 0.
    print txt_init  ; Print '.'.
    mvb tunix_io23, #3
    mvb tunix_blk1, #4
    ldy #0
    sty procs ; Unlink from free list.
    jsr fork0 ; Fork into process 0.
    ; Use new banks.
    get_procblk_y proc_ram123, ram123
    get_procblk_y proc_io23, io23
    sta tunix_io23
    get_procblk_y proc_blk1, blk1
    sta tunix_blk1
    sta blk1
    get_procblk_y proc_blk2, blk2
    get_procblk_y proc_blk3, blk3
    get_procblk_y proc_blk5, blk5

    ;; Save KERNAL vectors.
    stwi s, IOVECTORS
    stwi d, old_kernal_vectors
    stwi c, 30
    jsr memcpy

    ;; Replace KERNAL vectors.
    stwi s, tunix_vectors
    stwi d, IOVECTORS
    stwi c, 30
    jsr memcpy

    ;; Point devices to KERNAL.
    mvb drv_vl, #<old_kernal_vectors
    mvb drv_vh, #>old_kernal_vectors

    ;; Register TUNIX device.
    ldaxi tunix_driver
    ldy #TUNIX_DEVICE
    jmp register
.endproc

    .rodata

txt_tunix:  .byte 147   ; Clear screen.
            .byte "TUNIX", 13, 0
txt_tests:  .byte "RUNNING TESTS:",13, 0
txt_booting:.byte 13, "BOOTING..",0
txt_init:   .byte ".", 0

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

;;;;;;;;;;;;;
;;; TESTS ;;;
;;;;;;;;;;;;;

    .rodata

err_invalid_glfn_order:
    .byte "INVALID GLFN ORDER", 0
err_invalid_first_free_bank:
    .byte "INVALID FIRST FREE BANK", 0
err_invalid_num_free_procs:
    .byte "INVALID NUMBER OF FREE PROCS"
    .byte 0
err_fail:
    .byte "TEST FAILED", 0

txt_ex:.byte "CHILD",9

    .code

FREE_BANKS_AFTER_INIT = $6a ;MAX_BANKS - FIRST_BANK - 6

.export tests
.proc tests
.if 0
    jsr init
    ldaxi banks
    jsry list_length, free_bank
    cpx #FREE_BANKS_AFTER_INIT
    beq :+
    error err_fail

    ;; Doubly used list arrays.
    ; Pop bank from free list.
:   jsr balloc
    jsr bfree
    ldaxi banks
    jsry list_length, free_bank
    cpx #FREE_BANKS_AFTER_INIT
    beq :+
    error err_fail

    ;; Deque
    ; Allocate first.
:   lpopx procs, free_proc
    phx
    ldaxi procs
    jsry list_length, free_proc
    cpx #MAX_PROCS - 2 ; (+ init)
    beq :+
    error err_invalid_num_free_procs
:   plx
    ; Push onto running.
    dpushx procs, procsb, running
    ldaxi procs
    jsry list_length, running
    cpx #1
    beq :+
    error err_fail
 
    ;;; Syscalls
:
.endif
    jsr init

    ;; Fork
    lda #TUNIX_DEVICE
    tax
    jsr SETLFN
    lda #2
    ldx #<cmd_fork
    ldy #>cmd_fork
    jsr SETNAM
    jsr OPEN
    jsr BASIN
    pha
    jsr CLALL
    pla
    cmp #1
    beq :++
    cmp #0
    beq :+
    error err_fail

    ; Exit child.
:   print txt_ex
    lda #0
    lda #3
    ldx #<cmd_exit
    ldy #>cmd_exit
    jsr SETNAM
    jsr OPEN

    ; Move on with child.
:   lda #0
    jsr SETNAM
    jsr OPEN
    ; Wait for child.
    lda #1
    jsr wait

    rts
.endproc

cmd_fork:   .byte "PF"
cmd_exit:   .byte "PE", 0

.export halt
.proc halt
    jmp halt
.endproc

.export start
.proc start
    print txt_tunix
    print txt_tests
    jsr tests
    print txt_booting
    jmp init
.endproc

;;;;;;;;;;;;;;;;;
;;; DISPATCH ;;;;
;;;;;;;;;;;;;;;;;

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
    inc h+1
    lda drv_vh,y
    adc #0
    sta g+2
    sta h+2
g:  lda $ffff
    sta call_driver2+1
h:  lda $ffff
    sta call_driver2+2

    ;; Bank in driver BLK1.
    ldx drv_pid,y
    get_procblk_x proc_blk1, blk1
    load_regs
    jmp call_driver2
.endproc

; Translate local to global LFN.
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

.export open2
.proc open2
    lda LFN
    jsr lfn_to_glfn
    sta LFN
    tax

    ;; Assign driver to GLFN.
    ldy DEV
    lda dev_drv,y
    sta glfn_drv,x

    tax
    ldy glfn_drv,x
    jsra call_driver, #IDX_OPEN
    sta reg_a

    pop LFN
    popw FNADR
    php
    lda reg_a
    plp
    rts
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
    rts
:   sec
    rts
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
    rts
:   sec
    rts
.endproc

.macro iohandler name2, device, drvop
    .export name2
    .proc name2
        save_regs
        ldx device
        ldy dev_drv,x
        jsra call_driver, #drvop
        sta reg_a
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
    ldy glfn_drv,x
    jmpa call_driver, #IDX_CLOSE
.endproc

.export stop2
.proc stop2
    ldx #0
    ldy glfn_drv,x
    jmpa call_driver, #IDX_STOP
.endproc

.export usrcmd2
.proc usrcmd2
    ldx #0
    ldy glfn_drv,x
    jmpa call_driver, #IDX_STOP
.endproc

; Schedule task switch
; Picks next or first on running list.
.export schedule
.proc schedule
    php
    pha
    phx
    phy
    mvb blk1, tunix_blk1
    ldx pid
    lda procs,x
    bne :+
    lda running
:   cmp pid
    beq :+  ; Don't switch to self.
    jsr switch
:   ply
    plx
    pla
    plp
    rts
.endproc

io_load:

    .org $9800

io_start:

tunix_vectors:
.word open, close, chkin, ckout, clrcn
.word basin, bsout, stop, getin, clall
.word usrcmd, load, save, blkin, bkout

stmp:   .res 1

.export call_driver2
.proc call_driver2
j:  jsr $fffe

    ; Restore banks.
    php
    pha
    phx
    mvb blk1, tunix_blk1
    ldx pid
    get_procblk_x proc_blk2, blk2
    get_procblk_x proc_blk3, blk3
    get_procblk_x proc_blk5, blk5
    get_procblk_x proc_blk1, blk1
    plx
    pla
    plp
    rts
.endproc

.export save_banks_y
.proc save_banks_y
    set_procblk_y proc_ram123, ram123
    set_procblk_y proc_io23, io23
    set_procblk_y proc_blk1, blk1
    set_procblk_y proc_blk2, blk2
    set_procblk_y proc_blk3, blk3
    set_procblk_y proc_blk5, blk5
    rts
.endproc

.export load_banks_y
.proc load_banks_y
    get_procblk_y proc_ram123, ram123
    get_procblk_y proc_io23, io23
    get_procblk_y proc_blk2, blk2
    get_procblk_y proc_blk3, blk3
    get_procblk_y proc_blk5, blk5
    get_procblk_y proc_blk1, blk1
    rts
.endproc

; Switch to process.
; A: Process ID
.export switch
.proc switch
    ;;; Save current.
    tsx
    stx stack
    pha
    ldy pid
    jsr save_banks_y
    save_internal_ram_to_blk5_y

    ;; Load next.
    ply
    load_internal_ram_from_blk5_y
    jsr load_banks_y
    ldx stack
    txs
    rts
.endproc

.export tunix_enter
.proc tunix_enter
    pha
    mvb blk1, tunix_blk1
    pla
    rts
.endproc

.export tunix_leave
.proc tunix_leave
    php
    lda reg_a
    plp
    rts
.endproc

.export open
.proc open
    pushw FNADR
    push LFN

    ;; Copy file name.
    ldy FNLEN
    beq :++
    dey
:   lda (FNADR),y
    sta filename,y
    dey
    bpl :-
:   stwi FNADR, filename

    mvb blk1, tunix_blk1
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
iowrap stop, stop2
iowrap usrcmd, usrcmd2

.export clrcn
.proc clrcn
    jsr tunix_enter
    jmpa call_driver, #IDX_CLRCN
.endproc

.export clall
.proc clall
    ;; Close all open files.
    push blk1
    mvb blk1, tunix_blk1
    ldx first_lfn
    beq r
:   phx
    jsr close
    plx
    lnextx lfns, :-

    ;; Reset to standard I/O.
r:  mvb DFLTN, #0
    mvb DFLTO, #3
    pop blk1
    rts
.endproc

.macro blkiohandler name, idx
    .export name
    .proc name
        jsr tunix_enter
        save_regs
        ldy DEV
        ldx dev_drv,y
        jmpa call_driver, #idx
    .endproc
.endmacro

blkiohandler load, IDX_LOAD
blkiohandler save, IDX_SAVE

io_end:

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOCAL (per process) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

.export tunix_io23, tunix_blk1, lbanks, lbanksb, first_lbank, lfns, lfnsb, lfn_glfn, first_lfn, waiting, waiting_pid, free_wait, first_wait, pid, ppid, reg_a, reg_x, reg_y, stack, flags, saved_vic, filename, response, response_len, responsep

tunix_io23: .res 1
tunix_blk1: .res 1

;; Extended memory banks
; Deque of used ones.
lbanks:     .res MAX_BANKS
lbanksb:    .res MAX_BANKS
first_lbank:.res 1

;; Logical file numbers
; Deque of used ones
lfns:       .res MAX_LFNS
lfnsb:      .res MAX_LFNS
first_lfn:  .res 1
lfn_glfn:   .res MAX_LFNS

;; Process info
waiting:    .res MAX_PROCS
waitingb:   .res MAX_PROCS
waiting_pid:.res MAX_PROCS
free_wait:  .res 1
first_wait: .res 1
pid:        .res 1
ppid:       .res 1

;; CPU state
reg_a:      .res 1
reg_x:      .res 1
reg_y:      .res 1
flags:      .res 1
; Task-witching
stack:      .res 1

;; VIC
saved_vic:  .res 16

;; Syscalls
; File name copied from calling process.
filename:       .res 256
; TUNIX syscall device response
response:       .res 8
response_len:   .res 1
responsep:      .res 1
