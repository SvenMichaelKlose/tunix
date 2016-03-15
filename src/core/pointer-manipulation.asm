inc_s:
    inc s
    bcc +n
    inc @(++ s)
n:  rts

inc_d:
    inc d
    bcc +n
    inc @(++ d)
n:  rts
