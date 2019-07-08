.export ingle_exec, init_proc, alloc_proc, free_proc

.import save_state
.import launch
.import alloc_bank, free_bank

.data

NUM_PROCESSES = 32

procs:          .res NUM_PROCESSES
proc_ram:       .res NUM_PROCESSES
proc_ram123:    .res NUM_PROCESSES
proc_io23:      .res NUM_PROCESSES
proc_blk1:      .res NUM_PROCESSES
proc_blk2:      .res NUM_PROCESSES
proc_blk3:      .res NUM_PROCESSES
proc_blk5:      .res NUM_PROCESSES

.code

.proc init_proc
    ldx #NUM_PROCESSES
    lda #0
l1: sta procs,x
    dex
    bpl l1

    rts
.endproc

; Return new process in X.
.proc alloc_proc
    lda $9ff1
    pha
    and #%11111100
    ora #%00000011
    sta $9ff1
    lda $9ff4
    pha
    lda $9ff5
    pha
    lda #$7f
    sta $9ff4
    lda #$00
    sta $9ff5

    ldx #0
l1: lda procs,x
    beq got_slot
    inx
    cpx #NUM_PROCESSES
    bne l1
error:
    sec
    bcs return      ; (jmp)

got_slot:
    lda #1
    sta procs,x
    jsr alloc_bank
    bcs error
    sta proc_ram,x
    jsr alloc_bank
    bcs error
    sta proc_ram123,x
    jsr alloc_bank
    bcs error
    sta proc_io23,x
    jsr alloc_bank
    bcs error
    sta proc_blk1,x
    jsr alloc_bank
    bcs error
    sta proc_blk2,x
    jsr alloc_bank
    bcs error
    sta proc_blk3,x
    jsr alloc_bank
    bcs error
    sta proc_blk5,x

    clc
    jmp return
.endproc

.proc return
    pla
    sta $9ff5
    pla
    sta $9ff4
    pla
    sta $9ff1
    rts
.endproc

.proc ingle_exec
    lda $9ff1
    pha
    and #%11111100
    ora #%00000011
    sta $9ff1
    lda $9ff4
    pha
    lda $9ff5
    pha
    lda #$7f
    sta $9ff4
    lda #$00
    sta $9ff5

    jsr alloc_proc
    bcs return
    stx current_proc

    lda proc_ram123,x
    ldy #0
    sta $0124
    sty $0125
    lda proc_io23,x
    sta $0126
    sty $0127
    lda proc_blk1,x
    sta $0128
    sty $0129
    lda proc_blk2,x
    sta $012a
    sty $012b
    lda proc_blk3,x
    sta $012c
    sty $012d
    lda proc_blk5,x
    sta $012e
    sty $012f

    jmp launch
.endproc

; Free process X.
.proc free_proc
    lda $9ff1
    pha
    and #%11111100
    ora #%00000011
    sta $9ff1
    lda $9ff4
    pha
    lda $9ff5
    pha
    lda #$7f
    sta $9ff4
    lda #$00
    sta $9ff5

    lda procs,x
    bne ok
error:
    sec
    jmp return

ok: lda #0
    sta procs,x
    lda proc_ram,x
    jsr free_bank
    lda proc_ram123,x
    jsr free_bank
    lda proc_io23,x
    jsr free_bank
    lda proc_blk1,x
    jsr free_bank
    lda proc_blk2,x
    jsr free_bank
    lda proc_blk3,x
    jsr free_bank
    lda proc_blk5,x
    jsr free_bank

    clc
    jmp return
.endproc
