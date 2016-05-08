.export exec_script, next_bytecode, inc_sp

.importzp sp, sa, srx
.import syscall_vectors_l, syscall_vectors_h, syscall_args_l, syscall_args_h

.bss

num_args:   .byte 0

.code

.proc inc_sp
    inc sp
    bne n
    inc sp+1
n:  rts
.endproc

.proc inc_sa
    inc sa
    bne n
    inc sa+1
n:  rts
.endproc

.proc exec_script
    pla ; Y
    pla ; X
    sta srx
    pla ; A
    plp ; flags
    pla
    sta sp
    pla
    sta sp+1
    lda sp
    sec
    sbc #1
    sta sp
    bcs n
    dec sp+1
n:

next_bytecode:
    ; Get opcode.
    ldy #0
    lda (sp),y
    beq +done   ; Return to native code…

    ; Get pointer to argument info.
    tax
    dex
    lda syscall_vectors_l,x
    sta mod_call+1
    lda syscall_vectors_h,x
    sta mod_call+2
    lda syscall_args_l,x
    sta sa
    lda syscall_args_h,x
    sta sa+1

    ; Get number of arguments.
    lda (sa),y
    sta num_args

    ; Copy arguments to zero page.
next_arg:
    jsr inc_sp
    dec num_args
    bmi script_call

    jsr inc_sa
    lda (sa),y      ; Get zero page address from argument info.
    tax
    lda (sp),y      ; Copy in argument value.
    sta 0,x

    jmp next_arg

script_call:
mod_call:
    jsr $ffff
    jmp next_bytecode

    ; Return to native code.
done:
    lda sp+1
    pha
    lda sp
    pha
    ldx srx
    rts
.endproc

next_bytecode = exec_script::next_bytecode
