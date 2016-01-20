reset_region:
    brk
    c_setzs rxl 4 0 @(-- screen_width) 0 @(-- screen_height)
    0
    rts
