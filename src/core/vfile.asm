;; File VOPs.
VOP_READ = 0
VOP_WRITE = 2
;VOP_CLOSE_FILE = 4

;; Directory VOPs.
;VOP_OPEN_FILE = 0
;VOP_OPEN_DIR = 2
;VOP_CLOSE_DIR = 4
;VOP_LOOKUP = 6
;VOP_UPDATE = 8
;VOP_REMOVE = 10

; X: vfile index.
; Y: Operation index.
call_vfile_op:
    ; Save whatever is in A.
    sta tmp5

    ; Save current core.
    lda $9ff4
    pha

    lda #0
    sta $9ff4

    ; Get pointer to vfile operation vector table.
    lda vfile_ops_l,x
    sta tmp
    lda vfile_ops_h,x
    sta tmp2

    ; Fetch operation vectors that needs to be called.
    lda (tmp),y
    sta tmp3
    iny
    lda (tmp),y
    sta tmp4

    ; Restore A and call operation.
    lda tmp5
    jsr +l

    ; Save return value.
    sta tmp5

    ; Return to callee's core.
    pla
    sta $9ff4

    ; Restore return value.
    lda tmp5

    rts

l:  jmp (tmp3)
