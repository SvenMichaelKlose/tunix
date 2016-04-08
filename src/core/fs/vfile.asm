;; File VOPs.
VOP_READ = 0
VOP_WRITE = 2
VOP_LOOKUP = 4

; X: vfile index.
; Y: Operation index.
call_vfile_op:
    ; Save whatever is in A.
    sta tmp5

    ; Get pointer to vfile operation vector table.
    jsr take_over
    lda $9ff4
    pha
    lda #0
    sta $9ff4
    lda vfile_ops_l,x
    sta tmp
    lda vfile_ops_h,x
    sta tmp2
    pla
    sta $9ff4
    jsr release

    ; Fetch operation vectors that needs to be called.
    lda (tmp),y
    sta tmp3
    iny
    lda (tmp),y
    sta tmp4

    ; Restore A and call operation.
    lda tmp5
    jsr +l
    sta tmp5

    php
    pla
    sta tmp6

    lda tmp6
    pha
    lda tmp5
    plp
    rts

l:  jmp (tmp3)
