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
