pat_empty:
    .byte 0, 0, 0, 0, 0, 0, 0, 0

pat_solid:
    .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff

pat_background:

pat_leaves:
    .byte %11000000
    .byte %00100001
    .byte %00010010
    .byte %00001100
    .byte %00110000
    .byte %01000000
    .byte %10000000
    .byte %10000000
    .byte %11000000

pat_ovals:
    .byte %01000001
    .byte %00100010
    .byte %10011100
    .byte %00100010
    .byte %01000001
    .byte %10000000
    .byte %10000000
    .byte %10000000
    .byte %01000001

pat_woven:
    .byte %00011111
    .byte %00101110
    .byte %01000100
    .byte %11100010
    .byte %11110001
    .byte %11101000
    .byte %01000100
    .byte %10001110

pat_pits:
    .byte %11111011
    .byte %00000000
    .byte %11111011
    .byte %11111011
    .byte %00001010
    .byte %00001010
    .byte %00001010
    .byte %00001010

pat_gray:
    .byte $aa, $55, $aa, $55, $aa, $55, $aa, $55, $aa, $55

pat_smiley:
    .byte %00111100
    .byte %01000010
    .byte %10100101
    .byte %10100101
    .byte %10000001
    .byte %01011010
    .byte %00100100
    .byte %00011000
