; X: vfile
; s: CBM path name
devcbm_update_directory:
    lda vfile_handles,x
    sta tmp

    jsr strlen
    tya
    ldx s
    ldy @(++ s)
    jsr SETNAM

    ldx tmp
    lda devcbm_device_numbers,x
    tax
    ldy #$00    ; Read.
    lda #$02    ; Reserved logical file number.
    jsr SETLFS

    jsr OPEN
    bcs +error

    ldx #$02
    jsr CHKIN
    bcs +error

    ; Skip load address.
    jsr devcbm_read
    bcs +error
    jsr devcbm_read
    bcs +error

next_entry:
    jsr READST
    bne +done

    ; Skip address of next line.
    jsr devcbm_read
    bcs +error
    jsr devcbm_read
    bcs +error

    ; Read BASIC line number, which is the size in blocks.
    jsr devcbm_read
    bcs +error
    sta dirent_size
    jsr devcbm_read
    bcs +error
    sta @(+ 1 dirent_size)
    lda #0
    sta @(+ 2 dirent_size)
    sta @(+ 3 dirent_size)

l:  jsr devcbm_read
    bcs +error
    bne -l      ; continue until end of line

    jmp -next_entry

error:
    jmp return_cbm_error

done:
    lda #$02
    jsr CLOSE
    bcs -error

    jmp CLRCHN

devcbm_read:
    lda #0
    sta devcbm_eof
    jsr READST
    bne +eof

    jsr CHRIN
    bcs -error
    pha
    lda $90
    cmp #1   ; set carry when ST>0 (i.e., <>0!)
    pla      ; keep carry, and possibly set Z flag for byte=0
    rts

eof:
    inc devcbm_eof
    sec
    rts
