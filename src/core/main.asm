found_memory_expansion = $80

    org $a009

max_num_processes = 32
max_num_libraries_per_process = 8

main:
    sei
    cld
    lda #$7f
    sta $911d
    sta $911e

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

    ; Map in master core and init process bank.
    lda #%00111111   ; IO3/2 RAM, +3K R/W RAM
    sta $9ff1
    lda #%01000011   ; BLK5 ROM, BLK1 R/W RAM
    sta $9ff2
    ldx #0
    stx $9ff4
    inx
    stx $9ff8

    ; Clear +3K area.
    ldy #0
    sty d
    sty c
    lda #$04
    sta @(++ d)
    lda #$0d
    sta @(++ c)
    tya
l:  sta (d),y
    inc d
    bne +n
    inc @(++ d)
n:  dec c
    bne +n
    dec @(++ c)
    bne -l
n:

    ; Initialise copying the core to its intended location.
    ldx #@(- cinfo_core_end cinfo_core 1)
l:  lda cinfo_core,x
    sta s,x
    dex
    bpl -l

    ; Copy the core.
    jsr copy

    ; Initialise copying the init process to its intended location.
    ldx #@(- cinfo_init_end cinfo_init 1)
l:  lda cinfo_init,x
    sta s,x
    dex
    bpl -l

    ; Copy init process.
    jsr copy

    jmp relocated_init

copy:
    ldy #0
l:  lda (s),y
    sta (d),y
    inc s
    bne +n
    inc @(++ s)
n:  inc d
    bne +n
    inc @(++ d)
n:  dec c
    bne -l
    dec @(++ c)
    bne -l
    rts

kernal_size         = @(- kernal_end kernal)
loaded_kernal_end   = @(+ loaded_kernal (-- kernal_size))
kernal_end2         = @(-- kernal_end)

dispatched_core = @(+ (- loaded_kernal #x2000) #xa000)

cinfo:
cinfo_core:
    <dispatched_core >dispatched_core
    <kernal >kernal
    <kernal_size @(++ >kernal_size)
cinfo_core_end:

cinfo_init:
    $00 $a0
    $00 $20
    $00 $30
cinfo_init_end:
cinfo_end:

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

    org @(+ (- *pc* #xa000) #x2000)

relocated_init:
    ; Init RAM.
    ldx #15
l:  lda mem_init,x
    sta $9ff0,x
    dex
    bpl -l

    ;;; Initialise bank allocator.
    ; Set number of banks.
    lda found_memory_expansion
    beq +n
    lda #@(-- (/ 128 8 8))
    sta @(++ mod_max_banks)
    ; Pre–allocate bank of master core, init process and IO (for linking).
n:  lda #%00000111
    sta banks

    ;; Initialise init process.
    ; Set BLK1 bank.
    lda #1
    sta bank1
    sta saved_bank1

    ; Set up register contents.
    lda #<init
    sta saved_pc
    lda #>init
    sta @(++ saved_pc)
    tsx
    stx saved_sp

    ; Make copy of stack and init process
    ldx #0
l:  lda $100,x
    sta saved_stack,x
    lda loaded_init,x
    sta init,x
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
    %11111111   ; All BLKs R/W RAM
    0           ; (ID)
    0 0         ; +3K
    2 0         ; IO
    1 0         ; BLK 1
    0 0         ; BLK 2
    0 0         ; BLK 3
    0 0         ; BLK 5
