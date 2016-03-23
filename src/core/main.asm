max_num_processes = 32
max_num_libraries_per_process = 32

main:
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
    lda #<txt_no_memory
    ldy #>txt_no_memory
    jmp $cb1e
found_ultimem8m:
    lda #<txt_ultimem8m
    ldy #>txt_ultimem8m
    jmp +l
found_ultimem512k:
    inc found_memory_expansion
    lda #<txt_ultimem512k
    ldy #>txt_ultimem512k
l:  jsr $cb1e

    ; Init RAM.
    ldx #15
l:  lda mem_init,x
    sta $9ff0,x
    dex
    bpl -l

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
    ldx #@(- cinfo_end cinfo 1)
l:  lda cinfo,x
    sta s,x
    dex
    bpl -l

    ; Copy the core.
    ldy #0
l:  lda (s),y
    sta (d),y
    dec s
    lda s
    cmp #$ff
    bne +n
    dec @(++ s)
n:  dec d
    lda d
    cmp #$ff
    bne +n
    dec @(++ d)
n:  dec c
    bne -l
    dec @(++ c)
    bne -l

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

    ; Save old NMI vector.
    lda $0318
    sta old_nmi
    lda $0319
    sta @(++ old_nmi)

    ; Run it.
    jsr take_over
    lda #0
    jmp switch_to_process

found_memory_expansion:   0

kernal_size         = @(- kernal_end kernal)
loaded_kernal_end   = @(+ loaded_kernal (-- kernal_size))
kernal_end2         = @(-- kernal_end)

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

cinfo:
cinfo_kernal:
    <loaded_kernal_end >loaded_kernal_end
    <kernal_end2 >kernal_end2
    <kernal_size @(++ >kernal_size)
cinfo_kernal_end:
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
