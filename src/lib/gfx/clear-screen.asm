clear_screen:
    lda #$00
    sta d
    sta c
    lda #$10
    sta @(++ d)
    lda #$20
    sta @(++ c)
    jmp clrram
