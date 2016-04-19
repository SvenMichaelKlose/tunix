error:
    jmp return_cbm_error

; X: vfile
; s: CBM path name
devcbm_read_directory:
    stx tmp6
    lda vfile_handles,x
    sta tmp5

    jsr strlen
    tya
    ldx s
    ldy @(++ s)
    jsr SETNAM

    ldx tmp5
    lda devcbm_device_numbers,x
    tax
    ldy #$00    ; Read.
    lda #$02    ; Reserved logical file number.
    jsr SETLFS

    jsr stop_task_switching
    jsr OPEN
    jsr start_task_switching
    bcs -error

    ldx #$02
    jsr stop_task_switching
    jsr CHKIN
    jsr start_task_switching
    bcs -error

    ; Skip load address.
    jsr devcbm_read
    bcs -error
    jsr devcbm_read
    bcs -error

    ;; Skip first three lines.
    ldx #3
    ; Skip load address, first address of next line and line number.
m:  ldy #6
l:  jsr devcbm_read
    bcs -error
    dey
    bne -l

    ; Skip line.
    ldy #0
l:  jsr devcbm_read
    bcs -error
    bne -l      ; continue until end of line

    dex
    bne -m

    ; Get temporary bank.
    lda $9ffa
    pha
    jsr prepare_temporary_bank

    lda #<temporary_bank
    sta d
    lda #>temporary_bank
    sta @(++ d)

next_entry:
    lda d
    sta tmp2
    lda @(++ d)
    sta tmp3

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
    ldy #dirent_length
    sta (d),y
    jsr devcbm_read
    bcs +error
    iny
    sta (d),y

    ; Read until first double quote.
l:  jsr devcbm_read
    beq -next_entry
    cmp #$22
    bne -l

    ; Read name till quote.
    ldy #dirent_name
l:  jsr devcbm_read
    bcs +error
    beq -next_entry
    cmp #$22
    beq +n
    sta (d),y
    iny
    jmp -l
n:

    ; Read till end of line.
l:  jsr devcbm_read
    bne -l

    ; Step to next dirent and also save, so we can ignore the last line.
    lda d
    clc
    adc #dirent_size
    sta d
    bcc -next_entry
    inc @(++ d)
    jmp -next_entry

error2:
    pla
    jmp +l

error:
    jsr READST
    bne +done
l:  pla
    sta $9ffa
    jmp return_cbm_error

done:
    lda #$02
    jsr stop_task_switching
    jsr CLOSE
    jsr start_task_switching
    bcs -error

    jsr stop_task_switching
    jsr CLRCHN
    jsr start_task_switching

    lda $9ff8
    pha
    lda #BANK_DIRENTS
    sta $9ff8

    ; Write terminating zero.
    ldy #0
    tya
    sta (d),y

    ; Allocate area in dirent bank.
    lda tmp2
    clc
    adc #1
    sta c
    lda tmp3
    clc
    adc #0
    and #$1f
    sta @(++ c)
    jsr malloc
    bcs -error2

    ; Copy directory entries.
    ldx tmp6
    lda s
    sta vfile_data_l,x
    sta d
    lda @(++ s)
    sta vfile_data_h,x
    sta @(++ d)
    lda #<temporary_bank
    sta s
    lda #>temporary_bank
    sta @(++ s)
    lda #0
    jsr moveram

    pla
    sta $9ff8
    pla
    sta $9ffa
    rts

devcbm_read:
    jsr stop_task_switching
    lda #0
    sta devcbm_eof
    jsr READST
    bne +eof

    jsr CHRIN
    jsr start_task_switching
    bcs -error
    pha
    lda $90
    cmp #1   ; set carry when ST>0 (i.e., <>0!)
    pla      ; keep carry, and possibly set Z flag for byte=0
    rts

eof:
    inc devcbm_eof
    clc
    jmp start_task_switching
