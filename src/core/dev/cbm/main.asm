; X: vfile index
devcbm_read:
    jsr take_over

    lda vfile_handles,x
    tax
    lda devcbm_logical_file_numbers,x
    tax
    jsr chkin
    bcs +error
    jsr chrin
    bcs +error

    jmp release

devcbm_write:
    jsr take_over

    pha
    lda vfile_handles,x
    tax
    lda devcbm_logical_file_numbers,x
    tax
    jsr chkout
    bcs +error
    pla
    jsr chrout
    bcs +error

    jmp release

error:
    jmp set_cbm_error
