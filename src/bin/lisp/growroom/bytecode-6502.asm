.proc run_bytecode_fun
    ; Save pointer to bytecode object.
    sta bcp
    stx bcp+1

    ; Get pointer to argument definition.
    ldy #1
    lda (bcp),y
    sta argdef
    iny
    lda (bcp),y
    sta argdef+1

    ; Get number of stack places.
    iny
    lda (bcp),y
    pha
    and #$0f
    asl
    sta bcstacksize

    ; Get number of object pointers
    pla
    and #$f0
    lsr
    lsr
    lsr
    sta bcobjs

    ; Get offset into code.
    clc
    adc bcstacksize
    adc #3 ; Type + argdef
    adc bcp
    sta bcc
    lda bcp+1
    sta bcc+1
    bcc :+
    inc bcc+1
:

    ; Execute code
l:  ldy #0
    lda (bcc),y
    beq return
    bmi jumps

    ; Built-in
    ; Bytecode
    ; User

    ; Jump
    ; Jump if not NIL
    ; Jump if NIL
.endproc
