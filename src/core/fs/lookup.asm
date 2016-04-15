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
lookup_vfile:
    ; Prepare relative path.
    lda pwd
    sta tmp

    ldy #0
    lda (s),y
    cmp #@(char-code #\/)
    bne +not_absolute

    lda $9ff4
    pha
    lda #0
    sta $9ff4
    ldx vfile_root
    pla
    sta $9ff4
    sty tmp

not_absolute:
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
    sec
    rts

found:
    ldy #VOP_OPEN
    jmp call_vfile_op
