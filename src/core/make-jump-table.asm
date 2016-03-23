opcode_jmp = $4c
opcode_lda_imm = $a9
opcode_ldx_imm = $a2
opcode_ldy_imm = $a0

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

    lda end_of_library_calls
    sta tmp7
    lda @(++ end_of_library_calls)
    sta @(++ tmp7)

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

    lda do_make_jumps_to_core
    beq +n

    ; Make jump to core.
    lda (d),y
    tax
    iny
    lda (d),y
    ldy #2
    sta (tmp3),y
    dey
    txa
    sta (tmp3),y
    lda #opcode_jmp
    dey
    sta (tmp3),y
    jmp +l

    ; Make jump to library.
n:  lda (d),y
    sec
    sbc #1
    sta tmp5
    iny
    lda (d),y
    sbc #0
    sta tmp6

    ldy #0
    lda #opcode_jmp
    sta (tmp3),y
    iny
    lda tmp7
    sta (tmp3),y
    iny
    lda @(++ tmp7)
    sta (tmp3),y

    jsr make_library_call

    ; Step to next jump table entry.
l:  lda tmp3
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
    lda tmp7
    sta end_of_library_calls
    lda @(++ tmp7)
    sta @(++ end_of_library_calls)
    rts

library_core:   0

library_call:
    sta library_core
    lda $9ff4
    pha
    lda #>library_return
    pha
    lda #<library_return
    pha
    tya
    pha
    txa
    pha
    lda library_core
    sta $9ff4
    jmp switch_banks_in

library_return:
    pla
    php
    jsr switch_banks_in
    php
    rts
    
; Generate:
;   ldx #>fun
;   ldy #<fun
;   lda #library_core
;   jmp library_call
make_library_call:
    ldy #0

    lda #opcode_ldx_imm
    sta (tmp7),y
    iny
    lda tmp5
    sta (tmp7),y
    iny

    lda #opcode_ldy_imm
    sta (tmp7),y
    iny
    lda tmp6
    sta (tmp7),y
    iny

    lda #opcode_lda_imm
    sta (tmp7),y
    iny
    lda tmp2
    sta (tmp7),y
    iny

    lda #opcode_jmp
    sta (tmp7),y
    iny
    lda #<library_call
    sta (tmp7),y
    iny
    lda #>library_call
    sta (tmp7),y
    iny

    ; Add Y to tmp7.
    tya
    clc
    adc tmp7
    sta tmp7
    bcc +n
    inc tmp8
n:  rts
