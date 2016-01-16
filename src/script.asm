num_args:   0

exec_script:
    pla ; Y
    pla ; X
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
    ldy #0
    lda (sp),y
    beq +done
    tax
    dex
    lda syscall_args_l,x
    sta sa
    lda syscall_args_h,x
    sta @(++ sa)
    lda syscall_vectors_l,x
    sta @(+ 1 +mod_call)
    lda syscall_vectors_h,x
    sta @(+ 2 +mod_call)
    lda (sa),y
    sta num_args
next_arg:
    inc sa
    bne +n
    inc @(++ sa)
n:
    inc sp
    bne +n
    inc @(++ sp)
n:
    lda (sa),y
    tax
    lda (sp),y
    sta 0,x
    dec num_args
    bne next_arg

    inc sp
    bne +n
    inc @(++ sp)
n:

mod_call:
    jsr $ffff
    jmp exec_string

done:
    lda @(++ sp)
    pha
    lda sp
    pha
    rts
