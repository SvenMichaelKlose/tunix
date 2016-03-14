main:
    ; Welcome the user.
    lda #<txt_booting
    ldy #>txt_booting
    jsr $cb1e

    ; Check for Ultimem expansion.
    lda #<txt_checking_memory
    ldy #>txt_checking_memory
    jsr $cb1e
    lda $9f55
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
    lda #<txt_ultimem512k
    ldy #>txt_ultimem512k
l:  jsr $cb1e

    ; Init RAM.
    ldx #15
l:  lda mem_init,x
    sta $9ff0,x
    dex
    bpl -l

    ldx #@(- cinfo_end cinfo 1)
l:  lda cinfo,x
    sta s,x
    dex
    bpl -l

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

    jmp boot

kernal_size         = @(- kernal_end kernal)
loaded_kernal_end   = @(+ loaded_kernal (-- kernal_size))
kernal_end2         = @(-- kernal_end)

mem_init:
    %00000001   ; LED on.
    %00000011   ; +3K R/W RAM
    %11111111   ; All BLKs R/W RAM
    0           ; (ID)
    0 0         ; +3K
    1 0         ; IO
    2 0         ; BLK 1
    3 0         ; BLK 2
    4 0         ; BLK 3
    5 0         ; BLK 5

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
    @(ascii2petscii "CHECKING MEMORY...") 13 0

txt_ultimem8m:
    @(ascii2petscii "FOUND ULTIMEM 8M.") 13 0

txt_ultimem512k:
    @(ascii2petscii "FOUND ULTIMEM 512K.") 13 0

txt_no_memory:
    13
    @(ascii2petscii "ERROR: NO ULTIMEM EXPANSION FOUND.") 13 13
    @(ascii2petscii "CHECK HTTP://GO4RETRO.COM TO GET ONE.") 13 13
    @(ascii2petscii "FOR VICE PLEASE ADJUST YOUR CONFIGURATION.") 13 13
    @(ascii2petscii "SORRY. EXITING...") 13 0
