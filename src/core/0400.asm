    lda #1
    sta do_load_library

    ;;; Check if the core is requested.
    ldy #0
    lda (s),y
    cmp #@(char-code #\g)
    bne load_library
    iny
    lda (s),y
    beq load_library

    ; Make jump table to core.
    lda #<syscall_index
    sta c
    lda #>syscall_index
    sta @(++ c)
    jmp make_jump_table

error:
    rts

do_load_library:    0

load:
    lda #0
    sta do_load_library

load_library:
    ;;; Save current process' banks.
    lda $9ff4
    pha
    lda $9ff8
    pha
    lda $9ffa
    pha
    lda $9ffc
    pha
    lda $9ffe
    pha

    ;;; Save pointer to symbol list and want jump table.
    lda s
    pha
    lda @(++ s)
    pha
    lda d
    pha
    lda @(++ d)
    pha

    ;;; Open the library.
    jsr gopen
    bcs -error

    ;;; Allocate and populate +3K area.

    ; Get a bank.
    jsr alloc_bank

    ; Map it to $2000.
    lda tmp
    sta $9ff8

    ; Map +3K template to $4000.
    lda #0
    sta $9ffa

    ; Copy template to new bank.
    lda #$00
    sta s
    sta d
    sta c
    lda #$40
    sta @(++ s)
    lda #$20
    sta @(++ d)
    sta @(++ c)
    lda #0
    jsr moveram

    ;;; Map new 3K bank in.
    lda $9ff8
    sta $9ff4

    ;;; Save bank number.
    sta bank_ram

    ;;; Load index into upper half of +3K area.
    lda do_load_library
    beq +n
    lda #$00
    sta d
    lda #$30
    sta @(++ d)
    jsr gload
n:

    ;;; Allocate and assign blocks.
    ; Get destination address.
    jsr gchrin
    bcs +error2
    sta d
    jsr gchrin
    bcs +error2
    sta @(++ d)

    ; Load code size.
    jsr gwordin
    bcs error2

    ; Get first block.
    lda d
    bpl not_blk5
    ldy #4
    jmp +l
not_blk5:
    lsr
    lsr
    lsr
    lsr
    lsr
    tay

    ; Get number of blocks.
    lda c
    lsr
    lsr
    lsr
    lsr
    lsr
    tax

    ; Allocate banks and assign them to blocks.
l:  jsr alloc_bank
    sta bank1,y
    iny
    dex
    bpl -l

    ;;; Load code.
    jsr gloadn

    jsr gclose

    ;;; Make jump table for caller.
    lda do_load_library
    beq done_loading_program

    ; Restore pointers to symbol list and jump table.
    pla
    sta @(++ d)
    pla
    sta d
    pla
    sta @(++ s)
    pla
    sta s

    ; Step over library path.
    ldy #0
l:  lda (s),y
    beq +n
    jsr inc_s
    jmp -l

    ; Set pointer to index.
n:  lda #$00
    sta c
    lda #$30
    sta @(++ c)

    ; Make jump table.
    jsr make_jump_table

done_loading_program:
    clc
    jmp return_to_process

error2:
    jsr gclose
error:
    sec
    jmp return_to_process
