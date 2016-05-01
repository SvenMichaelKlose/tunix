core_size = @(- core_end core_start)

    <core_size >core_size

    org $a000

core_start:
    ; Configure Ultimem expansion.
    ldx #15
l:  lda mem_init,x
    sta $9ff0,x
    dex
    bpl -l

    ; Clear RAM bank 0.
    lda #$00
    sta d
    sta c
    lda #$20
    sta @(++ d)
    sta @(++ c)
    jsr clrram

    ;; Initialise memory bank allocator.
    ; Set number of banks.
    ldy #@(-- (/ 1024 8 8))
    lda found_memory_expansion
    beq +n
    ldy #@(-- (/ 128 8 8))
n:  sty max_banks

    ; Pre–allocate reserved banks. See 'src/core/mem/reserved-banks.asm'.
n:  lda #%00011111
    sta banks

    ; Initialise console.
    jsr devcon_init

    lda #<txt_welcome
    sta s
    lda #>txt_welcome
    sta @(++ s)
    jsr devcon_print_string

    ; Initialise dirent bank.
    lda #BANK_DIRENTS
    sta $9ff8
    jsr malloc_init

    ;; Initialise init process.
    jsr init_per_process_data

    ; Set up register contents.
    lda #<init
    sta saved_pc
    lda #>init
    sta @(++ saved_pc)
    tsx
    stx saved_sp

    ; Make copy of stack.
    ldx #0
l:  lda $100,x
    sta saved_stack,x
    inx
    bne -l

    ; Init process info.
    lda #129
    sta process_states

    ; Init linker.
    lda #<library_calls
    sta end_of_library_calls
    lda #>library_calls
    sta @(++ end_of_library_calls)

    ; Run it.
    jsr start_task_switching
    jsr take_over
    lda #BANK_CORE_INIT
    jmp switch_to_process

mem_init:
    %00000000   ; LED off.
    %00111111   ; IO3/2 RAM, +3K R/W RAM
    %11111111   ; BLK5 ro RAM, BLK1,2,3 R/W RAM
;    %10111111   ; BLK5 ro RAM, BLK1,2,3 R/W RAM
    0                   ; (ID)
    BANK_CORE_DATA 0    ; +3K
    0 0                 ; IO
    0 0                 ; BLK 1
    0 0                 ; BLK 2
    0 0                 ; BLK 3
    BANK_CORE_CODE 0    ; BLK 5

txt_welcome:
    "Welcome to g!" 10
    "Starting init process..." 10 0
