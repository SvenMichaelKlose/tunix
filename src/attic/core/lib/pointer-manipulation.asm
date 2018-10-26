inc_s:
    inc s
    bne +n
    inc @(++ s)
n:  rts

inc_d:
    inc d
    bne +n
    inc @(++ d)
n:  rts
