write:
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
    jmp call_vop

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

call_fop:
    lda vfile_ops_l,x
    sty tmp2
    clc
    adc #tmp2
    sta tmp
    lda vfile_ops_h,x
    adc #0
    sta tmp2
    jmp (tmp)

err_not_open:
err_not_writable:
    sec
    clc
