;__VIC20__ = 1
.include "cbm_kernal.inc"


    .zeropage

ptr:            .res 2
printptr:       .res 2
bnk:            .res 2
num_errors:     .res 2
col:            .res 1


    .code

.proc _main
    lda #<txt_welcome
    ldy #>txt_welcome
    jsr printstr

    ; Unhide UltiMem registers
    lda $9f55
    lda $9faa
    lda $9f01
    lda $9ff3
    cmp #$11
    beq has_ultimem

    lda #<txt_no_ultimem
    ldy #>txt_no_ultimem
    jmp printstr

has_ultimem:
    lda #<txt_found_ultimem
    ldy #>txt_found_ultimem
    jsr printstr

    lda #%11000000  ; RAM in BLK5.
    sta $9ff2


    lda #<txt_writing
    ldy #>txt_writing
    jsr printstr

    lda #0
    sta bnk
    sta bnk+1

l2: lda #$00
    sta ptr
    lda #$a0
    sta ptr+1
    lda bnk
    sta $9ffe
    lda bnk+1
    sta $9fff

l:  ldy #0
    lda bnk
    sta (ptr),y
    iny
    lda bnk+1
    sta (ptr),y
    inc ptr
    inc ptr
    bne l
    inc ptr+1
    lda ptr+1
    cmp #$c0
    bne l

    inc bnk
    lda bnk
    cmp #$80
    bne l2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda #<txt_reading
    ldy #>txt_reading
    jsr printstr

    lda #0
    sta bnk
    sta bnk+1
    sta col

l4: lda #$00
    sta ptr
    lda #$a0
    sta ptr+1
    lda bnk
    sta $9ffe
    lda bnk+1
    sta $9fff

l3: ldy #0
    lda bnk
    cmp (ptr),y
    bne error
    iny
    lda bnk+1
    cmp (ptr),y
    bne error
    inc ptr
    inc ptr
    bne l3
    inc ptr+1
    lda ptr+1
    cmp #$c0
    bne l3
    lda #'.'
    jsr printbnk

next_bank:
    inc bnk
    lda bnk
    cmp #$80
    bne l4

    lda num_errors
    bne has_errors
    lda #<txt_ram_ok
    ldy #>txt_ram_ok
    jmp printstr

has_errors:
    lda #<txt_error
    ldy #>txt_error
    jmp printstr

error:
    inc num_errors
    lda #'!'
    jsr printbnk
    jmp next_bank

got_ultimem:
    lda #<txt_found_ultimem
    ldy #>txt_found_ultimem
    jmp printstr
.endproc

.proc printbnk
    jsr BSOUT
    inc col
    lda col
    cmp #16
    bne r
    lda #13
    jsr BSOUT
    lda #0
    sta col
r:  rts
.endproc

.proc printstr
    sta printptr
    sty printptr+1

l:  ldy #0
    lda (printptr),y
    beq done
    jsr BSOUT
    inc printptr
    bne l
    inc printptr+1
    bne l

done:
    rts
.endproc


    .rodata

txt_welcome:
    .byte $93
    .byte "ULTIMEM RAM TEST", 13
    .byte "BY PIXEL@HUGBOX.ORG", 13
    .byte "   22 DEC 2021", 13
    .byte 13
    .byte 0

txt_found_ultimem:
    .byte "FOUND ULTIMEM.", 13
    .byte 13
    .byte 0

txt_no_ultimem:
    .byte "NO ULTIMEM OR VIC-MIDIFOUND.", 13
    .byte 0

txt_writing:
    .byte "WRITING RAM...", 13,0

txt_reading:
    .byte "READING RAM...", 13,0

txt_error:
    .byte "RAM SEEMS TO BE FAULTY.", 13,0

txt_ram_ok:
    .byte 13, "RAM OK.", 13,0
