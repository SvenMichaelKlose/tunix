.export _save_state
.importzp s, d, c
.import popax
.import ultimem_copy_ram2rom

.proc copy_bank
    ; Get source bank offset.
    ldx #0
    stx s+0
    stx s+1
    stx s+2
    ldx #5
l1: asl
    rol s+2
    dex
    bne l1
    sta s+1

    lda #$00
    sta c
    lda #$20
    sta c+1
    jmp ultimem_copy_ram2rom
.endproc

; void save_state (unsigned restart);
.proc _save_state
    ; Save return address.
    jsr popax
    sta $104
    stx $105

    ; Save VIC and Ultimem.
    ldx #$0f
l1: lda $9000,x
    sta $0106,x
    lda $9ff0,x
    sta $0116,x
    dex
    bpl l1

    ; Set restore marker.
    lda #'S'
    sta $100
    lda #'T'
    sta $101
    lda #'A'
    sta $102
    lda #'E'
    sta $103

    ; Copy main RAM.
    lda #0
    sta s
    sta s+1
    sta s+2
    sta s+3
    lda #0
    sta d
    lda #0
    sta d+1
    lda #$08
    sta d+2
    lda #0
    sta d+3
    lda #$00
    sta c
    lda #$20
    sta c+1
    jsr ultimem_copy_ram2rom

    lda $9ff4
    jsr copy_bank   ; $2000-$3fff
    lda $9ff6
    jsr copy_bank   ; $4000-$5fff
    lda $9ff8
    jsr copy_bank   ; $6000-$7fff
    lda #$a0
    sta s+1
    lda $9ffc
    jmp copy_bank   ; $a000-$bfff
.endproc
