.import _term_init, _term_put, _term_puts, _term_get
.import _ultimem_unhide, _ultimem_is_installed

.importzp s, d, c

LAST_HEAP_PAGE = $60

    .zeropage

a0:    .res 3  ; Arguments for built-in functions.
a1:    .res 3
a2:    .res 3
r:     .res 3  ; Return value of built-in function.

tmp:   .res 3
tmp2:  .res 3


    .code

    jmp _main


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

.proc err_fun_expected
.endproc

.proc err_too_many_args
.endproc

.proc err_out_of_memory
.endproc

    .rodata

txt_err_not_a_cons:
    .byte "Object is not a cons: ", 0

txt_err_out_of_memory:
    .byte "Object is not a cons: ", 0


F_ATOM          = %10000000
F_SYMBOL        = %10000000
F_NUMBER        = %10000001
M_TYPE          = %00000001

    .zeropage

heap:       .res 2
stack:      .res 2

    .code

; Allocate chunk of heap.
;
; A: Type info
; X: Size
.proc alloc_heap
    sta tmp2        ; Save type.

    ; Save pointer to object which this function
    ; returns.
l:  lda heap
    sta r
    lda heap+1
    sta r+1

    ; Save type byte.
    ldy #0
    lda tmp2
    sta (heap),y

    ; Step to next free byte for next allocation.
    txa
    clc
    adc heap
    sta heap
    bcs next_page
    rts

    ; Step to next heap page.
next_page:
    inc heap+1
    lda heap+1
    cmp #LAST_HEAP_PAGE
    bne done
    jmp err_out_of_memory

done:
    rts
.endproc


; #################
; ### TOP-LEVEL ###
; #################

    .code

.proc loop
    lda #<txt_prompt
    ldx #>txt_prompt
    jsr _term_puts

l:  jsr _term_get
    jsr _term_put
    jmp l
.endproc

    .rodata

txt_prompt:
    .byte "* ", 0


; ############
; ### INIT ###
; ############

    .code

.proc _main
    jsr _term_init

    lda #<txt_welcome
    ldx #>txt_welcome
    jsr _term_puts

;    jsr _ultimem_unhide
;    jsr _ultimem_is_installed
;    tax
;    bne ultimem_found

;    lda #<txt_no_ultimem
;    ldx #>txt_no_ultimem
;    jsr _term_puts

ultimem_found:
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

    ldx #31
    jsr alloc_heap

    jmp loop
.endproc

    .rodata

txt_t:
    .byte "t",0

txt_welcome:
    .byte "AttoLisp", 10, 13
    .byte "by Sven Michael Klose <pixel@hugbox.org>", 10, 13
    .byte "Welcome!", 10, 13
    .byte 10, 13, 0

txt_no_ultimem:
    .byte "No UltiMem expansion found. Stopped.", 0
