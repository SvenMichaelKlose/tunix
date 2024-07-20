.proc run_bytecode_fun
    ; Save pointer to bytecode object.
    sta bcp
    stx bcp+1

    ; Make stack frame.
    lda stack
    ldy #BCF_STACKSIZE
    sec
    sbc (bcp),y
    sta stack
    bcs :+
    dec stack+1
:

    ; Get offset into code.
    lda #BCF_CODEOFS
    lda (bcp),y ; # objects
    adc bcp
    sta bpc
    ldy bcp+1
    bcc :+
    iny
:   sty bpc+1

    ; Get code.
next:
    ldy #0
    lda (bpc),y
    beq return
    inc bpc
    bcc :+
    inc bpc+1
:
    ; Get function object.
    tay
    lda (bcp),y
    sta fun
    iny
    lda (bcp),y
    sta fun+1

    ; Get value of function.
    ldy #OFS_SYMBOL_VALUE
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
    sta bpc
    ldy bpc+1
    bcc :+
    iny
:   sty bpc+1
    bne l   ; (jmp)

next:
    inc bpc
    bne l
    inc bpc+1
    bne l
.endproc
