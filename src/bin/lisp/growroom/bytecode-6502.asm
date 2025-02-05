.proc get_place
    tay
    lsr
    tya
    bcs stackplace
    lda (bcp),y
    sta fun
    iny
    lda (bcp),y
    sta (++ fun)
    inc bcp
    inc bcp
    beq +
    inc (++ bcp)
+:  rts
stackplace:
    tay
    dey
    lda (stack),y
    sta fun
    iny
    lda (stack),y
    sta (++ fun)
    rts
.endproc

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
    beq :+
    inc bpc+1

    jsr get_place

    ; Get value of function.
    ldy #OFS_SYMBOL_VALUE
    lda (fun),y
    sta value
    iny
    lda (fun),y
    sta value

    cmp #TYPE_BUILTIN
    bne no_builtin

    ;;;;;;;;;;;;;;;;
    ;;; Built-in ;;;
    ;;;;;;;;;;;;;;;;

    ; Arguments to built-ins are copied to table 'args'.

    ldx #0  ; args
l:  ldy #0
    lda (bcc),y
    bmi last_arg
    jsr get_arg
    jmp -l
last_arg:
    jsr get_arg

    ; Call built-in
bi_call:
    jsr push_bcp
    jsr j
    jsr pop_bcp

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

error::

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
