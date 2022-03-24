.export _reset

.proc _reset
    ldx #end - start
l:  lda start,x
    sta $9c00,x
    dex
    bpl l

start:
    cld
    jsr $e518   ; Init VIC & clear screen
    jsr $e45b   ; Set BASIC vectors
    jsr $e3a4   ; Init zeropage
    jsr $e404   ; Print start message
    jmp $e381   ; Init SP & goto READY.
end:
.endproc
