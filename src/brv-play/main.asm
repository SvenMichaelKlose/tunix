.export _main

.import pushax

.importzp s

.code

.proc _main
    lda #<txt_welcome
    ldy #>txt_welcome
    jsr $cb1e

    lda #<txt_installing
    ldy #>txt_installing
    jsr $cb1e

    lda #2
    ldx #8
    ldy #2
    jsr $ffba   ; SETLF
    lda #fn_data_end-fn_data
    ldx #<fn_data
    ldy #>fn_data
    jsr $ffbd
    jsr $ffc0
    ldx #2
    jsr $ffc6

    lda #$00
    sta s
    lda #$a0
    sta s+1

l:  jsr $ffcf
    pha
    lda $90
    cmp #1
    pla
    bcc l

    jsr $ffcc
    lda #2
    jsr $ffc3

    rts
.endproc

.data

txt_welcome:
    .byte $93, "BRV PLAYER", 13, 0

txt_installing:
    .byte "LOADING VIDEO", 13, 0

fn_data:
    .byte "VIDEO.BRV,S,R"
fn_data_end:
