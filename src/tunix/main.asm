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

.export ram123, io23, blk1, blk2, blk3
.export blk5

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

.export FRESTOR, READST, SETLFN, SETNAM
.export OPEN, CLOSE, CHKIN, CKOUT, CLRCN
.export BASIN, BSOUT, LOAD, SAVE, SETTIM
.export RDTIM, STOP, GETIN, CLALL

PETSCII_CLRSCR = 147

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

;;; MACHDEP

IOPAGE_BASE  = $9b
MAX_IOPAGES  = 4

    .zeropage

;;; Registers

.export s, sl, sh, d, dl, dh
.export c, cl, ch

s:
sl:     .res 1
sh:     .res 1
d:
dl:     .res 1
dh:     .res 1
c:
cl:     .res 1
ch:     .res 1

ptr1:   .res 2
ptr3:   .res 2

;;;;;;;;;;;;;;
;;; GLOBAL ;;;
;;;;;;;;;;;;;;

    .bss

.export global_start, banks, free_bank
.export bank_refs, iopages, iopagesb
.export free_iopage, first_iopage
.export iopage_pid, iopage_page, glfns
.export glfn_refs, glfn_drv, procs
.export procsb, free_proc, running
.export sleeping, zombie, proc_flags
.export exit_codes, proc_ram123
.export proc_io23, proc_blk1, proc_blk2
.export proc_blk3, proc_blk5, drvs
.export drv_pid, drv_dev, drv_vl
.export drv_vh, dev_drv, copy_bank
.export global_end, global_size
.export global_start, banks_ok
.export banks_faulty

global_start:

tmp1:   .res 2
tmp2:   .res 2
tmp3:   .res 2

ptr2:   .res 2

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
proc_data:  .res MAX_PROCS
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
;
; Convenience macros to dampen down
; repetition.

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
; 'lpop*' and 'lnext*' also work with
; deques.
;
; There may be either multiple lists or
; deques in one set of arrays.

.macro linit list
    mvb list, #1
.endmacro

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

.macro alloc_proc_y
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
.macro add_waiting_y
    dallocy waiting, waitingb, free_wait, first_wait
.endmacro

; Remove process from waiting list.
.macro rm_waiting_y
    dmovey waiting, waitingb, first_wait,free_wait
.endmacro

; Make zombie a free process.

.macro rm_zombie_x
    dmovex procs, procsb, zombie, free_proc
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

.macro datax
    get_procblk_x proc_data, ram123
.endmacro

.macro datay
    get_procblk_y proc_data, ram123
.endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCESS CONTEXT (IO23) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.macro enter_context_x
    push io23
    io23x
    push ram123
    datax
.endmacro

.macro enter_context_y
    push io23
    io23y
    push ram123
    datay
.endmacro

.macro leave_context
    pop ram123
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

;;;;;;;;;;;;;;;;;;;;;;
;;; USER INTERFACE ;;;
;;;;;;;;;;;;;;;;;;;;;;
;
; Blah.

.export printstr
.proc printstr
    mvb tmp1, #0
    jsr CLRCN
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
;
; Generates memory moving speed code
; across multiple extended memory banks.
;
; ATM a block copy from BLK3 to BLK5 is
; created, starting with bank
; 'copy_bank'.

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
    print txt_speed_code
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
    print txt_newline
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

    .rodata

txt_speed_code:
    .byte "MAKING SPEED CODE...", 0
txt_newline:
    .byte 13, 0

;;;;;;;;;;;;;;;;;;;;;;
;;; MACHDEP VIC-20 :::
;;;;;;;;;;;;;;;;;;;;;;
;
; Process forks and switches.  TUNIX is
; not ready to support other platforms.

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
    mvb tmp1, blk1
    mvb tmp1+1, blk2
    mvb tmp2, blk3
    mvb tmp2+1, blk5
    mvb tmp3, ram123
    mvb blk1, tunix_blk1
    ldx pid
    lda proc_data,x
    sta tmp3+1

    ;; Make child's IO23 and use it.
    push io23 ; Save parent's.
    ; Allocate IO23 bank globally.
    lpopx banks, free_bank
    sta proc_io23,y
    inc bank_refs,x
    phx
    ; Copy parent's IO23 into child's.
    sta blk5
    ldaxi vec_io23_to_blk5
    jsr smemcpy
    ; Also lowmem, screen, color + VIC.
    save_internal_ram_to_blk5
    ; Allocate additional local.
    lpopx banks, free_bank ; (BLK5)
    pha
    sta proc_data,y
    inc bank_refs,x
    lda tmp3+1 ; (BLK3)
    jsr copy_blk3_to_blk5
    plx
    dpushx lbanks, lbanksb, first_lbank
    stx ram123 ; Map in addtional local.
    ; Map in child's IO23.
    pop io23
    sty pid  ; Set its PID.
    ; Register new IO23 locally.
    tax
    dpushx lbanks, lbanksb, first_lbank
    ; Set return stack.
    tsx
    inx ; (Undo 'push io23'.)
    sta stack

    ;; Copy remaining banks.
    ; Copies from BLK3 to BLK5 with
    ; speed code in BLK2.
    .macro forkblky procblk, srcblk
        jsr balloc ; (BLK5)
        sta procblk,y
        lda srcblk ; (BLK3)
        jsr copy_blk3_to_blk5
    .endmacro
    forkblky proc_ram123, tmp3
    forkblky proc_blk2, tmp1+1
    forkblky proc_blk3, tmp2
    forkblky proc_blk5, tmp2+1
    forkblky proc_blk1, tmp1

    cpy #0
    beq :+  ; Do not undo init proc.

    ;; Remove parent's active banks
    ;; from local list.
    ldx pid
.macro dereflbankx procblk
    ldy procblk,x
    jsr free_lbank
.endmacro
    dereflbankx proc_data
    dereflbankx proc_ram123
    dereflbankx proc_io23
    dereflbankx proc_blk1
    dereflbankx proc_blk2
    dereflbankx proc_blk3
    dereflbankx proc_blk5

    ;; Restore forked proc's banks.
:   mvb ram123, tmp3
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
; driver code.  In addition the RAM123
; area holds LFN data.

; Clone the current process.
; Returns:
; A: New process ID for the parent and
;    0 for the child.
.export fork
.proc fork
    ;; Grab process slot.
    alloc_proc_y
    cpy #0
    beq no_more_procs
.endproc

; Y: New process ID.
.export fork0
.proc fork0
    ;; Machine-dependend process copy.
:   phy
    jsr fork_raw
    ply

    ;; Increment bank refs.
    ldx first_lbank
    beq :++
:   inc bank_refs,x
    lnextx lbanks, :-

    ;; Increment GLFNs.
:   push ram123
    lda proc_data,y
    sta ram123
    ldx first_lfn
    beq :++
:   inc glfn_refs,x
    lnextx lfns, :-
:   pop ram123

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
    mv_running_sleeping_x
    clc
    rts
not_there:
already_sleeping:
    sec
    rts
.endproc

; Wake process up.
; A: Process ID.
.export resume
.proc resume
    lda proc_flags,x
    bpl not_to_resume
    mv_sleeping_running_x
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
    free_iopage_x
:   lnexty iopages, :--

    ;; Free drivers.
:.if 0
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
.endif
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
; Returns:
; A: Exit code
.export wait
.proc wait
    lda proc_flags,x
    beq not_there

    ;; Put us on waiting list.
    enter_context_x
    add_waiting_y
    lda pid
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
    enter_context_x
    ldy pid
    rm_waiting_y
    ldy first_wait
    bne resume_next_waiting
    rm_zombie_x
    jmp return_zombie_exit_code

resume_next_waiting:
    phx
    tya
    tax
    jsr resume
    plx

return_zombie_exit_code:
    leave_context
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
; Y:  device
; Returns:
; X:  driver ID or 0.
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
;
; Takes on system calls via device
; TUNIX_DEVICE (#31 by default).

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
    alloc_iopage_x
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
    free_iopage_x
    jmp respond_ok
not_there:
    jmp respond_error
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
    lda banks_ok
    jsr printnum
    print txt_banks_free
    rts

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

io_size = io_end - io_start

.export init
.proc init
    jsr FRESTOR
    jsr init_ultimem_banks

    ;; Clear global data.
    stwi d, global_start
    stwi c, global_size
    jsr bzero

    ;; Init local (per-process).
    stwi s, io_load
    stwi d, $9800
    stwi c, io_size
    jsr memcpy
    stwi d, io_end
    stwi c, $07f0-io_size
    jsr bzero
    stwi d, $0400
    stwi c, $0c00
    jsr bzero

    ;; Link lists.
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

    ;; Finish up lists.
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
    print txt_init
    ldx #2
    sta proc_data
    inx
    stx tunix_io23
    inx
    stx tunix_blk1
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

tunix_vectors:
.word open, close, chkin, ckout, clrcn
.word basin, bsout, stop, getin, clall
.word usrcmd, load, save, blkin, bkout

txt_tunix:
    .byte PETSCII_CLRSCR
    .byte "STARTING TUNIX", 13, 0
txt_tests:
    .byte "CHECKING SANITY.",13, 0
txt_init:
    .byte "STARTING INIT PROCESS."
    .byte 13, 0
txt_booting:
    .byte 13, "BOOTING..",0

;;;;;;;;;;;;;
;;; TESTS ;;;
;;;;;;;;;;;;;
;
; Tests running before regular boot.

    .rodata

err_invalid_glfn_order:
    .byte "INVALID GLFN ORDER", 0
err_invalid_first_free_bank:
    .byte "INVALID FIRST FREE BANK", 0
err_invalid_num_free_procs:
    .byte "INVALID NUMBER OF FREE PROCS"
    .byte 0
err_cannot_fork:
    .byte "CANNOT FORK", 0
err_fail:
    .byte "TEST FAILED", 0

txt_ex:
    .byte "CHILD",9

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
    bcc :+
    error err_cannot_fork
:   cmp #0
    bne :++

    ; Exit child.
:   print txt_ex
    lda #0
    lda #3
    ldx #<cmd_exit
    ldy #>cmd_exit
    jsr SETNAM
    jsr OPEN

    ; Wait for child to exit.
:   sta cmd_wait+3
    lda #0
    lda #3
    ldx #<cmd_wait
    ldy #>cmd_wait
    jsr SETNAM
    jsr OPEN
    rts
.endproc

cmd_fork:   .byte "PF"
cmd_exit:   .byte "PE", 0
cmd_wait:   .byte "PW", 0

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
;
; KERNAL I/O handlers with resident
; parts in IO23 that bank in BLK1
; (this place right here).

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

.export open2
.proc open2
    pushw FNADR
    stwi FNADR, filename
    push LFN
    jsr lfn_to_glfn
    sta LFN
    ldy DEV
    lda dev_drv,y
    tay
    jsra call_driver, #IDX_OPEN
    sta reg_a
    pop LFN
    popw FNADR
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
        save_regs
        ldx device
        ldy dev_drv,x
        jmpa call_driver, #drvop
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
    jmpa call_glfn_driver, #IDX_CLOSE
.endproc

.export clall2
.proc clall2
    ldx first_lfn
    beq :++
:   phx
    jsr close
    plx
    lnextx lfns, :-
:   jmp tunix_leave
.endproc

.export stop2
.proc stop2
    ldy #0
    jmpa call_driver, #IDX_STOP
.endproc

.export usrcmd2
.proc usrcmd2
    ldy #0
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
    jmp switch2
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Static KERNAL I/O handlers. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Always there for every process.

io_load:
    .org $9800  ; IO23
io_start:

.export call_driver2
.proc call_driver2
j:  jsr $fffe

    ; Restore banks.
    php
    pha
    phx
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

; Copy active bank numbers to process
; state.
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

; Copy active bank numbers from process
; state.
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

.export switch2
.proc switch2
    ;; Load next.
    ply
    load_internal_ram_from_blk5_y
    jsr load_banks_y
    ldx stack
    txs
    rts
.endproc

; Map in rest of TUNIX.
; It's in RAM123 and BLK1.  Saves
; current bank configuration to process
; state.
.export tunix_enter
.proc tunix_enter
    pha
    phx
    ldx pid
    ; Save active RAM123.
    lda ram123
    sta proc_ram123,x
    ; Save active BLK1.
    lda blk1
    sta proc_blk1,x
    ; Set additional local on RAM123.
    lda proc_data,x
    sta ram123
    plx
    ; Map in global to BLK1.
    mvb blk1, tunix_blk1
    pla
    rts
.endproc

; Restore process bank configuration.
; Only the areas TUNIX occupies (RAM123
; and BLK1).
.export tunix_leave
.proc tunix_leave
    php
    phx
    ldx pid
    lda proc_blk1,x
    sta blk1
    lda proc_ram123,x
    sta ram123
    plx
    lda reg_a
    plp
    rts
.endproc

.export open
.proc open
    ;; Move filename + pointer to IO23.
    ldy FNLEN
    beq :++
    dey
:   lda (FNADR),y
    sta filename,y
    dey
    bpl :-

:   jsr tunix_enter
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
    jmpa call_driver, #IDX_CLRCN
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
;
; The data part of the IO23 area.

.export tunix_io23, tunix_blk1, lbanks
.export lbanksb, first_lbank, lfns
.export lfnsb, lfn_glfn, first_lfn
.export waiting, waiting_pid, free_wait
.export first_wait, pid, reg_a
.export reg_x, reg_y, stack, flags
.export saved_vic, filename, response
.export response_len, responsep

    .bss

;; Vitals
tunix_io23:     .res 1  ; Per-process.
tunix_blk1:     .res 1  ; Same for all.
pid:            .res 1

;; Machine state
reg_a:          .res 1
reg_x:          .res 1
reg_y:          .res 1
flags:          .res 1
stack:          .res 1
saved_vic:      .res 16

;; Syscalls
filename:       .res 256
response:       .res 8
response_len:   .res 1
responsep:      .res 1

.if * >= IOPAGE_BASE * 256
.error "IO23 overflow!"
.endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Additional local bank ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    .bss
    .org $0400  ; RAM123

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

.if (IOPAGE_BASE + MAX_IOPAGES) * 256 > $a000
.error "IO pages overflow!"
.endif
