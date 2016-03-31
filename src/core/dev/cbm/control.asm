; Based on http://codebase64.org/doku.php?id=base:reading_the_directory
send_cbm_control_command:
    ; Get length of command.
    stx tmp
    sty tmp2
    ldy #0
l:  lda (tmp),y
    beq +n
    iny
    jmp -l

m:  tya
    ldy tmp2
    jsr SETNAM

    lda #$0f
    ldx #$08       ; default to device number 8
    ldy #$0f
    jsr SETLFS

    jsr OPEN
    bcs +error

    lda #$0f
    jsr CLOSE
    jsr CLRCHN
    clc
error:
    rts
