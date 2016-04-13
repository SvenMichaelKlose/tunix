; X: vfile index
devcbm_read:
    jsr stop_task_switching

    lda vfile_handles,x
    tax
    lda devcbm_logical_file_numbers,x
    tax
    jsr chkin
    bcs +error
    jsr chrin
    bcs +error

    jmp start_task_switching

devcbm_write:
    jsr stop_task_switching

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

    jmp start_task_switching

error:
    jmp set_cbm_error
