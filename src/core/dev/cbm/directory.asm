; Based on http://codebase64.org/doku.php?id=base:reading_the_directory
devcbm_open_directory:
;    lda #@(- dirname_end dirname)
;    ldx #<dirname
;    ldy #>dirname
    jsr SETNAM

    lda #$02
    ldx #$08       ; default to device number 8
    ldy #$00       ; secondary address 0 (required for dir reading!)
    jsr SETLFS

    jsr OPEN
    bcs +error

    LDX #$02
    jsr CHKIN
    bcs +error

    ; Skip load address.
    jsr devcbm_read
    bcs +error
    jsr devcbm_read
    bcs +error
    rts

devcbm_read_directory:
    LDX #$02
    jsr CHKIN
    bcs +error

    ; Skip address of next line.
    jsr devcbm_read
    bcs +error
    jsr devcbm_read
    bcs +error

    ; Read BASIC line number, which is the size in blocks.
    jsr devcbm_read
    bcs +error
    sta dirent_size
    jsr devcbm_read
    bcs +error
    sta @(+ 1 dirent_size)
    lda #0
    sta @(+ 2 dirent_size)
    sta @(+ 3 dirent_size)

l:  jsr devcbm_read
    bcs +error
    bne -l      ; continue until end of line
    rts

error:
    jmp return_cbm_error

done:
    lda #$02
    jsr CLOSE
    bcs -error

    jmp CLRCHN

devcbm_read:
    lda #0
    sta devcbm_eof
    jsr READST
    bne +eof

    jsr CHRIN
    bcs -error
    pha
    lda $90
    cmp #1   ; set carry when ST>0 (i.e., <>0!)
    pla      ; keep carry, and possibly set Z flag for byte=0
    rts

eof:
    inc devcbm_eof
    sec
    rts
