; Modes for fs_open() according to cc65's stdlib.
O_RDONLY = $01  ; Writeable.
O_WRONLY = $02  ; Readable.
O_RDWR   = $03  ; Both writable and readable.
O_CREAT  = $10  ; Create file.
O_TRUNC  = $20  ; On O_CREAT, truncate the file to 0 if it already exists.
O_APPEND = $40  ; Append to end of file.
O_EXCL   = $80  ; Ensure that O_CREAT creates a new file.

alloc_file:
    ldy #0
l:  lda file_states,y
    beq +done
    iny
    cpy #max_num_files_per_process
    bne -l
    lda #EMFILE
    sec
    rts
done:
    clc
    rts
    
; A: vfile
;
; Returns:
; Y: file
assign_vfile_to_file:
    ; Allocate file.
    tax
    jsr alloc_file
    bcs +r

    ; Assign vfile.
    txa
    sta file_vfiles,y
    lda #FILE_OPENED
    sta file_states,y

    ; Increment reference counts up to root vfile.
    lda $9ff4
    pha
    lda #0
    sta $9ff4

l:  inc vfile_refcnts,x
    lda vfile_parents,x
    beq +l
    tax
    jmp -l

l:  pla
    sta $9ff4

r:  rts

fs_create:
    rts

; s: Path name
; A: mode
fs_open:
    sta fs_mode

stop:
    lda s
    sta d
    lda @(++ s)
    sta @(++ d)
    jsr lookup_vfile
    pha
    bcc +n

    lda fs_mode
    and #O_CREAT
    beq +err_enoent

    jsr fs_create
    bcc fs_open
    pla
    rts

n:  pla
    jmp assign_vfile_to_file

err_enoent:
    pla
    lda #ENOENT
    jmp set_error

fs_close:
    clc
    rts

; X: File handle
;
; Returns:
; A: Byte.
fs_read:
    lda file_states,x
    tay
    and #FILE_OPENED
    beq +err_not_open

    tya
    and #FILE_READABLE
    beq +err_not_readable

    lda file_vfiles,x
    tax
    ldy #VOP_READ
    jmp call_vfile_op

err_not_open:
err_not_readable:
    sec
    rts

; A: Byte
; X: File handle
fs_write:
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
    sec
    pla
    rts
