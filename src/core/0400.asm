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

load_library:
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
    lda @(++ tmp)
    sta $9ff9

    ; Map +3K template to $4000.
    lda #0
    sta $9ffa
    sta $9ffb

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
    jsr @(+ #x2000 (- #x0400 switch_to_new_3k))

    ;;; Save bank number.
    lda $9ff4
    sta bank_ram
    lda $9ff5
    sta @(++ bank_ram)

    ;;; Load index into upper half of +3K area.
    lda #$00
    sta d
    lda #$30
    sta @(++ d)
    jsr gload

    ;;; Load rest of library like a regular program, so it gets linked.
    jsr gchrin
    bcs +error2
    sta d
    jsr gchrin
    bcs +error2
    sta @(++ d)
    jsr gload

    jsr gclose

    ;;; Make jump table for caller.
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
    jmp make_jump_table

error2:
    jsr gclose
error:
    sec
    rts

switch_to_new_3k:
    lda $9ff8
    sta $9ff4
    lda $9ff9
    sta $9ff5
    rts
