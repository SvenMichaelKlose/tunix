reset_region:
    brk
    c_setzs rxl 4 0 0 @(-- screen_width) @(-- screen_height)
    0
    rts
