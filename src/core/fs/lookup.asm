init_path_component_ptr:
    lda #<path_component
    sta s
    lda #>path_component
    sta @(++ s)
    rts

get_path_component:
    jsr init_path_component_ptr

    ldy #0
l:  lda (d),y
    beq +done
    cmp #@(char-code #\/)
    beq +done
    sta (s),y
    jsr inc_s
    jsr inc_d
    jmp -l

done:
    sta (s),y
    jsr inc_d
    jmp init_path_component_ptr

; d: Path.
;
; Returns:
; A: vfile
lookup_vfile:
    ;; Get vfile to start with.
    ; Prepare for relative path.
    lda pwd
    sta tmp

    ; Check if path is absolute.
    ldy #0
    lda (s),y
    cmp #@(char-code #\/)
    bne +not_absolute

    ; Start with root vfile.
    lda $9ff4
    pha
    lda #0
    sta $9ff4
    ldx vfile_root
    pla
    sta $9ff4
    stx tmp

    ;; Step along vfiles for each path component.
not_absolute:
    lda (d),y
    cmp #@(char-code #\/)
    bne +l
    jsr inc_d
l:  jsr get_path_component
    lda path_component
    beq +done
    ldy #VOP_LOOKUP
    lda tmp
    jsr call_vfile_op
    bcs +error

    sta tmp
    jmp -l

done:
    lda tmp
    clc
    rts

error:
    rts

; Default lookup in dirent cache.
;
; X: vfile
; s: name
vfile_default_lookup:
    lda $9ff8
    pha
    lda #BANK_DIRENTS
    sta $9ff8

    lda vfile_data_l,x
    sta d
    lda vfile_data_h,x
    sta @(++ d)

l:  ldy #0
    lda (d),y
    beq +not_found
    jsr strcmp
    beq +found
    lda d
    clc
    adc #dirent_size
    sta d
    bcc -l
    inc @(++ d)
    jmp -l

not_found:
    pla
    sta $9ff8
    lda #ENOENT
    sec
    rts

found:
    pla
    sta $9ff8
    ldy #VOP_OPEN
    jmp call_vfile_op

; Add empty dirent to vfile.
;
; X: vfile
;
; Returns:
; d: Pointer to empty dirent.
vfile_add_dirent:
    lda $9ff8
    pha
    lda #BANK_DIRENTS
    sta $9ff8

    lda vfile_data_l,x
    sta d
    sta tmp
    lda vfile_data_h,x
    sta @(++ d)
    sta tmp2

    ;; Fetch size of dirent list.
    ;; TODO: Fix! May be larger that intended!
    lda tmp
    sec
    sbc #malloc_chunk_header_size
    sta tmp
    bcs +n
    dec tmp2
n:  ldy #0
    lda (tmp),y
    sta c
    pha
    iny
    lda (tmp),y
    pha
    sta @(++ c)

    ;; Allocate larger chunk for dirent list.
    lda c
    clc
    adc #dirent_size
    sta c
    bcc +n
    inc @(++ c)
n:  jsr malloc

    ;; Make vfile point to new chunk.
    lda s
    sta vfile_data_l,x
    lda @(++ s)
    sta vfile_data_h,x

    ;; Copy dirent list into new chunk.
    pla
    sta @(++ c)
    pla
    sta c
    lda #0
    jsr moveram

    pla
    sta $9ff8
    rts
