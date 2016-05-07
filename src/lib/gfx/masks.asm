.export masks_left, masks_right, maskd_left, maskd_right

.data

masks_left:
    .byte %11111111
maskd_right:
    .byte %01111111
    .byte %00111111
    .byte %00011111
    .byte %00001111
    .byte %00000111
    .byte %00000011
    .byte %00000001
maskd_left:
    .byte %00000000
masks_right:
    .byte %10000000
    .byte %11000000
    .byte %11100000
    .byte %11110000
    .byte %11111000
    .byte %11111100
    .byte %11111110
    .byte %11111111
