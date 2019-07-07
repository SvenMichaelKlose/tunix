.export _main

.importzp s, d, c
.import moveram

.code

.proc _main
    lda #<(binary+2)
    sta s
    lda #>(binary+2)
    sta s+1
    lda #$00
    sta d
    lda #$a0
    sta d+1
    lda #$00
    sta c
    lda #$20
    sta c+1
    lda #0
    jsr moveram
    jmp 41000
.endproc

binary: .incbin "writenow.bin"
