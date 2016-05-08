.export exec_script, next_bytecode, inc_bcp

.importzp bcp, bca, srx
.import syscall_vectors_l, syscall_vectors_h, syscall_args_l, syscall_args_h

.bss

num_args:   .byte 0

.code

.proc inc_bcp
    inc bcp
    bne n
    inc bcp+1
n:  rts
.endproc

.proc inc_bca
    inc bca
    bne n
    inc bca+1
n:  rts
.endproc

.proc exec_script
    pla ; Y
    pla ; X
    sta srx
    pla ; A
    plp ; flags
    pla
    sta bcp
    pla
    sta bcp+1
    lda bcp
    sec
    sbc #1
    sta bcp
    bcs n
    dec bcp+1
n:

next_bytecode:
    ; Get opcode.
    ldy #0
    lda (bcp),y
    beq done    ; Return to native code…

    ; Get pointer to argument info.
    tax
    dex
    lda syscall_vectors_l,x
    sta mod_call+1
    lda syscall_vectors_h,x
    sta mod_call+2
    lda syscall_args_l,x
    sta bca
    lda syscall_args_h,x
    sta bca+1

    ; Get number of arguments.
    lda (bca),y
    sta num_args

    ; Copy arguments to zero page.
next_arg:
    jsr inc_bcp
    dec num_args
    bmi script_call

    jsr inc_bca
    lda (bca),y     ; Get zero page address from argument info.
    tax
    lda (bcp),y     ; Copy in argument value.
    sta 0,x

    jmp next_arg

script_call:
mod_call:
    jsr $ffff
    jmp next_bytecode

    ; Return to native code.
done:
    lda bcp+1
    pha
    lda bcp
    pha
    ldx srx
    rts
.endproc

next_bytecode = exec_script::next_bytecode
