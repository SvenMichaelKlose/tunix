; Based on http://codebase64.org/doku.php?id=base:reading_the_directory
cbm_list:
    lda #@(- dirname_end dirname)
    ldx #<dirname
    ldy #>dirname
    jsr SETNAM

    lda #$02
    ldx $BA
    bne +n
    ldx #$08       ; default to device number 8
n:  ldy #$00       ; secondary address 0 (required for dir reading!)
    jsr SETLFS

    jsr OPEN
    bcs +error

    LDX #$02
    jsr CHKIN

    ldy #$04       ; skip 4 bytes on the first dir line
    bne +m
loop:
    ldy #$02       ; skip 2 bytes on all other lines
m:  jsr getbyte    ; get a byte from dir and ignore it
    dey
    bne -m

    jsr getbyte    ; get low byte of basic line number
    tay
    jsr getbyte    ; get high byte of basic line number
    pha
    tya            ; transfer Y to X without changing Akku
    tax
    pla
    jsr $ddcd      ; print basic line number
    lda #$20       ; print a space first
l:  jsr CHROUT
    jsr getbyte
    bne -l      ; continue until end of line

    lda #$0D
    jsr CHROUT
    jsr CBM_STOP   ; RUN/STOP pressed?
    bne loop      ; no RUN/STOP -> continue
error:
    ; Akkumulator contains BASIC error code

    ; most likely error:
    ; A = $05 (DEVICE NOT PRESENT)
done:
    lda #$02
    jsr CLOSE

    jmp CLRCHN

getbyte:
    jsr READST
    bne +e         ; read error or end of file
    jmp CHRIN

e:  pla            ; don't return to dir reading loop
    pla
    jmp -done

dirname:
    "$"             ; filename used to access directory
dirname_end:
