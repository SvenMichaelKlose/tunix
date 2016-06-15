.proc link
    ldy #1
    sty do_load_library
    dey
    sty do_make_jumps_to_core

    ;;; Check if the core is requested.
    lda (s),y
    cmp #'/'
    bne load_library
    iny
    lda (s),y
    cmp #'g'
    bne load_library
    iny
    lda (s),y
    bne load_library

    ;;; Link to core.
    inc do_make_jumps_to_core

    ; Skip over library name.
    jsr inc_s
    jsr inc_s
    jsr inc_s

    ; Make jump table to core.
    lda #<syscall_index
    sta c
    lda #>syscall_index
    sta c+1
    jsr take_over
    jsr make_jump_table
    jmp release
.endproc

; s: Path of program to load.
;
; Returns:
; X: Core of program.
.proc load
    jsr take_over

    lda #0
    sta do_load_library
    jmp +l

error2:
    jsr set_cbm_error
    lda do_load_library
    beq +n
    pla
    pla
    pla
    pla
n:  jsr gclose
    bcc return_to_process
error:
    jsr set_cbm_error
return_to_process:
    pla
    sta $9ff6
    pla
    sta $9ff4
    jmp release

err_nomem:
    lda #ENOMEM
    jmp release_with_error

load_library:
    jsr take_over

    ;;; Save current process' banks.
l:  lda $9ff4
    pha
    lda $9ff6
    pha

    ;;; Save pointer to symbol list and wanted jump table.
    lda do_load_library
    beq +n
    lda s
    pha
    lda s+1
    pha
    lda d
    pha
    lda d+1
    pha
n:

    ;;; Open the library.
    jsr gopen
    bcs -error

    ;;; Allocate and populate +3K area.
    ; Get a bank.
    jsr alloc_bank
    bcs -err_nomem
    lda tmp
    sta $9ff4
    sta tmp2
    sta saved_bank_ram
    sta tmp7    ; For generating library calls.

    ;;; Initialise per–process data in +3K area.
    beq +n
    jsr init_per_process_data
n:

    ;;; Load index into upper half of +3K area bank and map it to IO.
    lda do_load_library
    beq +n
    lda #BANK_TEMPORARY
    sta $9ff6
    lda #$00
    sta d
    lda #$98
    sta d+1
    jsr readm
n:

    ;;; Allocate and assign blocks.
    ; Read destination address.
    jsr read
    bcc +n
e:  jmp -error2
n:  sta d
    sta program_start
    jsr read
    bcs -e
    sta d+1
    sta program_start+1

    ; Read code size.
    jsr readw
    bcs -e

    ; Read data size.
    lda c
    pha
    lda c+1
    pha
    jsr readw
    bcs -e
    lda c
    sta bss_size
    lda c+1
    sta bss_size+1
    pla
    sta c+1
    pla
    sta c

    ; Get first block.
    lda d+1
    bpl not_blk5
    ldx #0
    ldy #3
    jmp +l
not_blk5:
    lsr
    lsr
    lsr
    lsr
    lsr
    lsr
    tay

    ; Get total number of blocks.
    lda c
    clc
    adc bss_size
    lda @(++ c)
    adc @(++ bss_size)
    lsr
    lsr
    lsr
    lsr
    lsr
    lsr
    tax
    inx

    ; Allocate banks and assign them to blocks.
l:  jsr alloc_bank
    bcc +ok
    ; TODO: Free allocated banks.
    lda #ENOMEM
    jmp release_with_error
ok: lda tmp
    sta saved_bank1,y
    iny
    dex
    bpl -l
    jsr switch_banks_in

    ;;; Load code.
    jsr readn

    jsr gclose
    lda do_load_library
    beq done_loading_program

    ;;; Make jump table for caller.
    ; Restore pointers to symbol list and jump table.
    pla
    sta d+1
    pla
    sta d
    pla
    sta s+1
    pla
    sta s

    ; Get callee's banks.
    pla
    sta $9ff6
    pla
    ldy $9ff4
    sta $9ff4
    ldx process_slot
    sty $9ff4
    stx process_slot
    sta $9ff4

    ; Step over library path.
    ldy #0
l:  lda (s),y
    beq +n
    jsr inc_s
    jmp -l
n:  jsr inc_s

    ; Set pointer to index.
    lda #$00
    sta c
    lda #$98
    sta c+1

    ; Bank index in.
    lda #BANK_TEMPORARY
    sta $9ff6

    ; Make jump table.
    jsr make_jump_table

    ; Put library on process' list of libraries.
    lda tmp2
    beq +n
    ldx num_libraries
    sta libraries,x
    inc num_libraries
n:

    jsr release
    tax
    clc
    rts

done_loading_program:
    ldx tmp2    ; Core bank of loaded program.
    clc
    jmp return_to_process
