; AttoLisp


.importzp s, d, c
.import _term_init, _term_put, _term_puts, _get_key


    .zeropage

a0:         .res 3  ; Arguments to built-in functions.
a1:         .res 3
a2:         .res 3
r:          .res 3  ; Return value of built-in function.



; ############
; ### INIT ###
; ############

    .code

.proc _main
    jsr _term_init

    lda #<txt_welcome
    ldx #>txt_welcome
    jsr _term_puts

    lda #$00
    sta heap
    lda #$60
    sta heap+1

    lda #$00
    sta stack
    lda #$40
    sta stack+1

;    lda #<txt_t
;    ldx #>txt_t
;    jsr make_symbol
;    lda r
;    sta symbol_t
;    lda r+1
;    sta symbol_t+1

    jmp loop
.endproc

    .data

txt_t:
    .byte "t",0

txt_welcome:
    .byte "AttoLisp v0.1 2021-10-03", 13
    .byte "by Sven Michael Klose", 13
    .byte "   <pixel@hugbox.org>", 13
    .byte 13
    .byte "Welcome!", 13
    .byte "* "
    .byte 0


; #################
; ### TOP-LEVEL ###
; #################

    .code

.proc loop
l:  jsr _get_key
    jsr _term_put
    jmp l
.endproc


; ############
; ### HEAP ###
; ############

; The heap is steadily growing in BLK3.  An object address
; also comes with its bank number, so each pointer is three
; bytes in size.
; The MSB in the first object byte distinguishes atoms and
; conses.  If it is set, the object is an atom and the seven
; bits left contain its size.

F_ATOM      = %10000000
F_SYMBOL    = %11000000
F_NUMBER    = %10100000
M_SIZE      = %00011111

    .zeropage

heap:       .res 3
stack:      .res 3

    .code

.proc swap_stack_and_heap
    lda stack
    ldx heap
    sta heap
    stx stack
    lda stack+1
    ldx heap+1
    sta heap+1
    stx stack+1
    lda stack+2
    ldx heap+2
    sta heap+2
    stx stack+2
    rts
.endproc

; X: Size
.proc alloc_heap
    lda heap+1
    cmp #7          ; Last page of heap bank?
    beq check       ; Check low pointer...

    ; Save pointer to object.
l:  lda heap
    sta r
    lda heap+1
    sta r+1
    lda heap+2
    sta r+2

    ; Step to next free space.
    txa
    and #M_SIZE
    clc
    adc heap
    sta heap
    lda heap+1
    adc #0
    sta heap+1

    rts

check:
    txa
    clc
    adc heap
    bcc l

    ; Switch to next free bank.
    inc heap+2
    lda heap+2
    sta $9ffc

    ; Reset heap pointer to start of bank.
    lda #$00
    sta heap
    lda #$60
    sta heap+1

    bne l       ; (jmp)
.endproc


; #############
; ### ATOMS ###
; #############


    .code

; X: Flags/size
.proc alloc_atom
    jsr alloc_heap

    ; Set atom flag.
    ldy #0
    txa
    sta (r),y

    rts
.endproc


; ##############
; ### CONSES ###
; ##############

    .code

.proc alloc_cons
    lda #6
    jsr alloc_heap

    ldy #0
    lda a0
    sta (r),y
    iny
    lda a0+1
    sta (r),y
    iny
    lda a0+2
    sta (r),y
    iny
    lda a1
    sta (r),y
    iny
    lda a1+1
    sta (r),y
    iny
    lda a1+2
    sta (r),y

    rts
.endproc

.proc car
    lda a0+3
    sta $9ffc

    ldy #0
.endproc

.proc cdx
    lda (a0),y
    stx r
    and #128
    bne e
    iny
    lda (a0),y
    sta r+1
    iny
    lda (a0),y
    sta r+2

    rts

e:  jmp err_not_a_cons
.endproc

.proc cdr
    lda a0+3
    sta $9ffc

    ldy #3
    bne cdx
.endproc

.proc rplaca
    lda a1+3
    sta $9ffc

    ldy #0
.endproc

.proc rplacx
    lda (a1),y
    and #128
    bne err_not_a_cons

    lda a0
    sta (a1),y
    iny
    lda a0+1
    sta (a1),y
    sta r+1
    iny
    lda a0+2
    sta (a1),y
    sta r+2

    rts
.endproc

.proc rplacd
    lda a1+3
    sta $9ffc

    ldy #3
    bne rplacx      ; (jmp)
.endproc


; ###############
; ### SYMBOLS ###
; ###############

    .zeropage

symbol_t:       .res 3
sp_root:        .res 3  ; Root node of symbol name tree
sp:             .res 2  ; Symbol name tree pointer
symbol_name:    .res 2  ; Symbol name string


    .code

.proc symbol_length
    rts
.endproc

.proc find_symbol
    rts
.endproc

.proc insert_symbol
    rts
.endproc

.proc add_symbol
    jsr symbol_length
    ora #F_SYMBOL
    tax
    jsr alloc_atom

    ; Copy symbol name.
    lda r
    clc
    adc #3
    sta d
    lda r+1
    adc #0
    sta d+1
    ldy #0
l:  lda symbol_name,y
    sta (d),y
    beq n
    iny
    jmp l

n:  jmp insert_symbol
.endproc

.proc make_symbol
    stx symbol_name
    sta symbol_name+1
    jsr find_symbol
    bcs r
    jsr add_symbol
r:  rts
.endproc


; ###############
; ### NUMBERS ###
; ###############

    .zeropage

number:     .res 2


    .code

.proc make_number
    ldx #F_NUMBER + 3
    jsr alloc_atom

    ldy #0
    lda number
    sta (r),y
    iny
    lda number+1
    sta (r),y

    rts
.endproc


; ############
; ### READ ###
; ############


; ##############
; ### ERRORS ###
; ##############

    .code

.proc err_not_a_cons
    lda #<txt_err_not_a_cons
    ldx #>txt_err_not_a_cons
    jsr _term_puts
    jmp loop
.endproc

    .data

txt_err_not_a_cons:
    .byte "Object is not a cons: ", 0
