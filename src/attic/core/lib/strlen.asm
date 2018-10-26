; s: String
;
; Returns:
; Y: length, excluding terminating zero.
;
; Destroys: A
strlen:
    ldy #0
l:  lda (s),y
    beq +n
    iny
    jmp -l
n:  rts
