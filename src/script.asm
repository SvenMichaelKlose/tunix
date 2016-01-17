num_args:   0

inc_sp:
    inc sp
    bne +n
    inc @(++ sp)
n:  rts

inc_sa:
    inc sa
    bne +n
    inc @(++ sa)
n:  rts

exec_script:
    pla ; Y
    pla ; X
    sta srx
    pla ; A
    plp ; flags
    pla
    sta sp
    pla
    sta @(++ sp)
    lda sp
    sec
    sbc #1
    sta sp
    bcs +n
    dec @(++ sp)
n:

exec_string:
    ; Get opcode.
    ldy #0
    lda (sp),y
    beq +done   ; Return to native code…

    ; Get pointer to argument info.
    tax
    dex
    lda syscall_vectors_l,x
    sta @(+ 1 +mod_call)
    lda syscall_vectors_h,x
    sta @(+ 2 +mod_call)
    lda syscall_args_l,x
    sta sa
    lda syscall_args_h,x
    sta @(++ sa)

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
    jmp exec_string

    ; Return to native code.
done:
    lda @(++ sp)
    pha
    lda sp
    pha
    ldx srx
    rts

    ; Call system function without argument mapping.
apply:
    lda (sp),y
    tax
    dex
    jsr inc_sp
    lda syscall_vectors_l,x
    sta @(+ 1 +mod_call)
    lda syscall_vectors_h,x
    sta @(+ 2 +mod_call)
mod_call:
    jmp $ffff

addx:
    lda tmp
    clc
    adc xpos
    sta xpos
    rts

addy:
    lda tmp
    clc
    adc ypos
    sta ypos
    rts

setzb:
    ldx tmp
    lda tmp2
    sta 0,x
    rts

setzw:
    ldx tmp
    lda tmp2
    sta 0,x
    lda tmp3
    sta 1,x
    rts

addzb:
    ldx tmp2
    ldy tmp3
    lda 0,x
    clc
    adc 0,y
    ldx tmp
    sta 0,x
    rts

sbczb:
    ldx tmp2
    ldy tmp3
    lda 0,x
    sec
    sbc 0,y
    ldx tmp
    sta 0,x
    rts

sbczbi:
    ldx tmp
    lda 0,x
    sec
    sbc tmp2
    sta 0,x
    rts

