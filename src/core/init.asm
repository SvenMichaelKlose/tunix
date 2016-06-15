.import clrram, devcon_init, devcon_print_string
.import init_per_process_data, switch_to_process, start_task_switching, take_over
.import malloc_init
.import banks, found_memory_expansion, process_states
.import end_of_library_calls
.importzp s, d, c
.importzp max_banks
.importzp saved_pc, saved_sp

.init

.word core_end-core_start

;    org $a000

.proc core_start
    jsr init_memory
    jsr init_console
    jsr init_fs
    jmp start_init_process

.proc init_memory
    ; Configure Ultimem expansion.
    ldx #15
l:  lda mem_init,x
    sta $9ff0,x
    dex
    bpl l

    ; Clear RAM bank 0.
    lda #$00
    sta d
    sta c
    lda #$20
    sta d+1
    sta c+1
    jsr clrram

    ;; Initialise memory bank allocator.
    ; Set number of banks.
    ldy #1024/8/8-1
    lda found_memory_expansion
    beq n
    ldy #128/8/8-1
n:  sty max_banks

    ; Pre–allocate reserved banks. See 'src/core/mem/reserved-banks.asm'.
    lda #%00011111
    sta banks
    rts
.endproc

.proc init_console
    jsr devcon_init

    lda #<txt_welcome
    sta s
    lda #>txt_welcome
    sta s+1
    jmp devcon_print_string
.endproc

.proc init_fs
    lda #BANK_DIRENTS
    sta $9ff8
    jmp malloc_init
.endproc

.proc start_init_process
    jsr init_per_process_data

    ; Set up register contents.
    lda #<init
    sta saved_pc
    lda #>init
    sta saved_pc+1
    tsx
    stx saved_sp

    ; TOOD: Required?
    ; Make copy of stack.
    ldx #0
l2: lda $100,x
    sta saved_stack,x
    inx
    bne l2

    ; Init process info.
    lda #129        ; TODO: flags PS_ALLOCATED, PS_RUNNING
    sta process_states

    ; Init linker.
    lda #<library_calls
    sta end_of_library_calls
    lda #>library_calls
    sta end_of_library_calls+1

    ; Run it.
    jsr start_task_switching
    jsr take_over
    lda #BANK_CORE_INIT
    jmp switch_to_process
.endproc

mem_init:
    .byte %00000000         ; LED off.
    .byte %00111111         ; IO3/2 RAM, +3K R/W RAM
    .byte %11111111         ; BLK5 ro RAM, BLK1,2,3 R/W RAM
;.byte    %10111111          ; BLK5 ro RAM, BLK1,2,3 R/W RAM
    .byte 0                 ; (ID)
    .byte BANK_CORE_DATA, 0 ; +3K
    .word 0                 ; IO
    .word 0                 ; BLK 1
    .word 0                 ; BLK 2
    .word 0                 ; BLK 3
    .word BANK_CORE_CODE     ; BLK 5

txt_welcome:
    .byte "Welcome to g!", 10
    .byte "Starting init process...", 10, 0
