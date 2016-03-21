try_next:
    ;;; Step to next entry in index.
    ; Step over name.
    ldy #0
l:  lda (tmp),y
    inc tmp
    bne +n
    inc @(++ tmp)
n:  tax
    bne -l

    ; Step over address' high byte.
    lda tmp
    clc
    adc #2
    sta tmp
    bcc +compare
    inc @(++ tmp)
    jmp +compare

; Inputs:
; s; List of wanted jump table entries as ASCIIZ strings.
; d: Where the jump table should go.
; c: Index of library.
make_jump_table:
get_entry:
    ; Get base address of library's index.
    lda c
    sta tmp
    lda @(++ c)
    sta @(++ tmp)

    ; Check if jump table is complete.
    ldy #0
    lda (s),y
    beq +done

    ; Compare ASCIIZ string.
compare:
    lda (s),y
    cmp (tmp),y
    bne -try_next
    iny
    cmp #0
    bne -compare

    ; Make jump.
    lda (tmp),y
    tax
    iny
    lda (tmp),y
    ldy #2
    sta (d),y
    dey
    txa
    sta (d),y
    lda #$4c    ; Opcode for JMP.
    dey
    sta (d),y

    ; Step to next jump table entry.
    lda d
    clc
    adc #3
    sta d
    bcc +n
    inc @(++ d)
n:

    ; Step to next wanted symbol.
    ldy #0
l:  lda (s),y
    jsr inc_s
    tax
    bne -l
    jmp -get_entry

done:
    rts
