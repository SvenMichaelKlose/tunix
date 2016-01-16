vmaskd: %01111111
        %10111111
        %11011111
        %11101111
        %11110111
        %11111011
        %11111101
        %11111110

vmasks: %10000000
        %01000000
        %00100000
        %00010000
        %00001000
        %00000100
        %00000010
        %00000001

; SYSCALL: Draw a vertical line
;
; xpos:     top X position
; ypos:     top Y position
; height:   -
; pattern:  8-byte pattern to draw
vline:
    lda xpos
    and #7
    tax
    lda vmasks,x
    sta masks
    lda vmaskd,x
    sta maskd
