.export _main

.code

.proc _main
    lda #<txt_welcome
    ldy #>txt_welcome
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

txt_welcome:
    .byte $93, "G INSTALLER", 13, 0

fn_data:
    .byte "GDATA.BIN,S,R"
fn_data_end:
