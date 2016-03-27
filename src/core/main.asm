max_num_processes = 32
max_num_libraries_per_process = 8
max_num_files_per_process = 8
max_num_vfiles = 256

found_memory_expansion = $80

    org $a000

    ; ROM autostart info.
    <main >main     ; Cold start vector
    <main >main     ; Warm start vector
    "A0"
    $c3 $c2 $cd     ; "CBM"

    ;; Ultimem file system info.
    "ULTIFS" 0              ; ID
    $01 $00                 ; File system version.

    ; Block store info.
    @*fs-block-size*        ; Logical logarithmic block size (1Kb).
    @(low *fs-blocks*)      ; Number of logical blocks.
    @(high *fs-blocks*)
    $01 $00                 ; Physical block of block allocation map.
relative_ultifs_bam = @(- ultifs_bam #xa000)
    <relative_ultifs_bam    ; Address of BAM.
    >relative_ultifs_bam
    $02 $00                 ; Physical first block (8Kb sized).

    ; File store info.
    $02 $00                 ; First physical block of file allocation map.
relative_ultifs_fam = @(- ultifs_bam #xa000)
    <relative_ultifs_fam    ; Address of BAM.
    >relative_ultifs_fam


ultifs_bam:
    1
    fill @(- *img-blocks* 2)

main:
    sei
    lda #$7f
    sta $911d
    sta $911e

    cld
    ldx #$ff
    txs

    jsr $fd8d   ; Init memory.
    jsr $fd52   ; Init KERNAL I/O vectors.
    jsr $fdf9   ; Init VIAs.
    jsr $e518

    ; Welcome the user.
    lda #<txt_booting
    ldy #>txt_booting
    jsr $cb1e

    ; Check for Ultimem expansion.
    lda #<txt_checking_memory
    ldy #>txt_checking_memory
    jsr $cb1e
    lda $9f55       ; Unhide registers.
    lda $9faa
    lda $9f01
    lda $9ff3
    cmp #$11
    beq found_ultimem8m
    cmp #$12
    beq found_ultimem512k

    ; No suitable memory extension found – cannot boot.
    lda #<txt_no_memory
    ldy #>txt_no_memory
    jsr $cb1e
    jsr $ffd2       ; Wait for keypress.
    jmp ($c000)     ; Start BASIC.

found_ultimem8m:

    lda #<txt_ultimem8m
    ldy #>txt_ultimem8m
    jmp +l

found_ultimem512k:
    inc found_memory_expansion
    lda #<txt_ultimem512k
    ldy #>txt_ultimem512k
l:  jsr $cb1e

    ; Configure Ultimem expansion.
    ldx #15
l:  lda mem_init,x
    sta $9ff0,x
    dex
    bpl -l

    ;; Initialise memory bank allocator.
    ; Set number of banks.
    lda found_memory_expansion
    beq +n
    lda #@(-- (/ 128 8 8))
    sta @(++ mod_max_banks)
    ; Pre–allocate bank of master core and IO (for linking).
n:  lda #%00000011
    sta banks

    jsr init_per_process_data

    ;; Initialise init process.
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

    ; Run it.
    jsr take_over
    lda #0
    jmp switch_to_process

mem_init:
    %00000000   ; LED off.
    %00111111   ; IO3/2 RAM, +3K R/W RAM
    %01111111   ; BLK5 ROM, BLK1,2,3 R/W RAM
    0           ; (ID)
    0 0         ; +3K
    0 0         ; IO
    0 0         ; BLK 1
    0 0         ; BLK 2
    0 0         ; BLK 3
    0 0         ; BLK 5

txt_booting:
    $93 @(ascii2petscii "BOOTING G...") 13 0

txt_checking_memory:
    @(ascii2petscii "CHECKING MEMORY...") 0

txt_ultimem8m:
    @(ascii2petscii "ULTIMEM 1024K RAM.") 13 0

txt_ultimem512k:
    @(ascii2petscii "ULTIMEM 128K RAM.") 13 0

txt_no_memory:
    13
    @(ascii2petscii "ERROR: NO ULTIMEM EXPANSION FOUND.") 13 13
    @(ascii2petscii "CHECK HTTP://GO4RETRO.COM TO GET ONE.") 13 13
    @(ascii2petscii "FOR VICE PLEASE ADJUST YOUR CONFIGURATION.") 13 13
    @(ascii2petscii "SORRY. EXITING...") 13 0
