.proc run_bytecode_fun
    ; Save pointer to bytecode object.
    sta bcp
    stx bcp+1

    ; Make stack frame.
    lda stack
    ldy #1
    sec
    sbc (bcp),y
    sta stack
    bcs :+
    dec stack+1
:

    ; Get offset into code.
    iny
    lda (bcp),y ; # objects
    clc
    adc bcstacksize
    asl
    adc #4 ; Type, len & argdef
    adc bcp
    sta bcc
    ldy bcp+1
    bcc :+
    inc bcc+1
:   sty bcc+1

    ; Execute code
l:  ldy #0
    lda (bcc),y
    beq return
    sta op
    bmi jumps

    ; Built-in
    ; Bytecode
    ; User

jumps:
    asl
    bpl do_jmp
    asl
    bmi jmp_nil

    ; Jump if not NIL
    lda last
    ora last+1
    bne do_jmp
    beq next    ; (jmp)

    ; Jump if NIL
jmp_nil:
    lda last
    ora last+1
    bne next

do_jmp:
    lda op
    and #31
    tay
    lda bcp
    clc
    adc (bcp),y
    sta bcc
    ldy bcc+1
    bcc :+
    iny
:   sta bcc+1
    bne l   ; (jmp)

next:
    inc bcc
    bne l
    inc bcc+1
    bne l
.endproc
