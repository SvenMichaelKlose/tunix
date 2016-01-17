reset_region:
    brk
    c_setzb rxl 50
    c_setzb rxr @(-- screen_width)
    c_setzb ryt 50
    c_setzb ryb @(-- screen_height)
    0
    rts
