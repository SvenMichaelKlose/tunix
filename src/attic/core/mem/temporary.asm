temporary_bank = $4000

prepare_temporary_bank:
    lda #BANK_TEMPORARY
    sta $9ffa
    lda #$00
    sta d
    sta c
    lda #>temporary_bank
    sta @(++ d)
    lda #$20
    sta @(++ c)
    jmp clrram
