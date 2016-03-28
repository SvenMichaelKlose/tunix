; A: Byte
; X: File handle
write:
    pha
    lda file_states,x
    tay
    and #FILE_OPENED
    beq +err_not_open

    tya
    and #FILE_WRITABLE
    beq +err_not_writable

    lda file_vfiles,x
    tax
    ldy #VOP_WRITE
    pla
    jmp call_vfile_op

err_not_open:
err_not_writable:
    pla
    sec
    rts

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
    sta tmp5
    lda $9ff4
    pha
    lda #0
    sta $9ff4
    lda vfile_ops_l,x
    sta tmp
    lda vfile_ops_h,x
    sta tmp2
    lda (tmp),y
    sta tmp3
    iny
    lda (tmp),y
    sta tmp4
    lda tmp5
    jsr +l
    pla
    sta $9ff4
    rts

l:  jmp (tmp3)
