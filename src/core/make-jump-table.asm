try_next:
    ;;; Step to next entry in index.
    ; Step over name.
    ldy #0
l:  lda (d),y
    inc d
    bne +n
    inc @(++ d)
n:  tax
    bne -l

    ; Step over address.
    jsr inc_d
    jsr inc_d
    jmp +compare

; Inputs:
; s; List of wanted jump table entries as ASCIIZ strings.
; d: Where the jump table should go.
; c: Index of library.
make_jump_table:
    lda d
    sta tmp3
    lda @(++ d)
    sta @(++ tmp3)

get_entry:
    ; Get base address of library's index.
    lda c
    sta d
    lda @(++ c)
    sta @(++ d)

    ; Check if jump table is complete.
    ldy #0
    lda (s),y
    beq +done

    ; Compare ASCIIZ string.
compare:
    jsr compare_asciiz
    bne -try_next

    ; Make jump.
    lda (d),y
    tax
    iny
    lda (d),y
    ldy #2
    sta (tmp3),y
    dey
    txa
    sta (tmp3),y
    lda #$4c    ; Opcode for JMP.
    dey
    sta (tmp3),y

    ; Step to next jump table entry.
    lda tmp3
    clc
    adc #3
    sta tmp3
    bcc +n
    inc @(++ tmp3)
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
