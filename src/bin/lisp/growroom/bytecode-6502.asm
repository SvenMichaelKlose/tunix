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

    ; Get code.
next:
    ldy #0
    lda (bcc),y
    beq return
    inc bcc
    bcc :+
    inc bcc+1
:
    ; Get function object.
    asl
    tay
    lda (bcp),y
    sta fun
    iny
    lda (bcp),y
    sta fun+1

    ; Get value of function.
    ldy #IDX_SYMBOL_VALUE
    lda (fun),y
    sta value
    iny
    lda (fun),y
    sta value

    ;;;;;;;;;;;;;;;;
    ;;; Built-in ;;;
    ;;;;;;;;;;;;;;;;

    cmp #TYPE_BUILTIN
    bne no_builtin

    ldx #0  ; args
l:  ldy #0
    lda (bcc),y
    bmi bi_call
    asl
    bmi bi_const

    ; Stack arg
    tay
    lda (sp),y
    iny
    sta args,x
    inx
    lda (sp),y
    sta args,x
    inx
    inc bcc
    bcc l
    inc bcc+1
    bne l   ; (jmp)

    ; Constant list argument
bi_const:
    and #$7f
    tay
    lda (bcp),y
    iny
    sta args,x
    inx
    lda (bcp),y
    sta args,x
    inx
    inc bcc
    bcc l
    inc bcc+1
    bne l   ; (jmp)

    ; Call built-in
bi_call:
    jsr push_bcp
    jsr j
    jsr pop_bcp

set_place:
    sta value

    ; Get stack index.
    ldy #0
    lda (bcc),y
    tay

    inc bcc
    bcc :+
    inc bcc+1
:
    ; Copy value to stack.
    lda value
    sta (sp),y
    iny
    txa
    sta (sp),y
    jmp next

j:  jmp (value)

no_builtin:

    ;;;;;;;;;;;;;;;;
    ;;; Bytecode ;;;
    ;;;;;;;;;;;;;;;;
    ; Same as built-in but copying arguments to the GC
    ; stack instead of 'args'..
no_bytecode:

    ;;;;;;;;;;;;
    ;;; User ;;;
    ;;;;;;;;;;;;
    ; Get start of argument definition.
    ; Save and set symbol values.

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
