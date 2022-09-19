.export _reset

    .code

.proc _reset
    ldx #end - start
l:  lda start,x
    sta $9d00,x
    dex
    bpl l
    jmp $9d00

start:
    lda #$ff
    sta $9ff2
    ldx #4
    lda #0
    stx $9ff8
    sta $9ff9
    inx
    stx $9ffa
    sta $9ffb
    inx
    stx $9ffc
    sta $9ffd
    inx
    stx $9ffe
    sta $9fff

    cld
    jsr $e518   ; Init VIC & clear screen.
    jsr $e45b   ; Set BASIC vectors.
    jsr $e3a4   ; Init zero page.
    jsr $e404   ; Print welcome message.
    jmp $e381   ; BASIC cold start.
end:
.endproc
