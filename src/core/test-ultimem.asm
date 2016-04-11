found_memory_expansion = $80

test_ultimem:
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
l:  jmp $cb1e

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
