c_calcscr=1
c_setpattern=2
c_vline=3
c_hline=4
c_frame=5
c_box=6
c_putstring=7
c_putchar=8
c_setzb=9
c_setzw=10
c_setzs=11
c_addzb=12
c_sbczb=13
c_sbczbi=14
c_addx=15
c_addy=16
c_pushz=17
c_popz=18
c_apply=19
syscall_vectors_l: .byte <calcscr, <setpattern, <vline, <hline, <frame, <box, <putstring, <putchar, <setzb, <setzw, <setzs, <addzb, <sbczb, <sbczbi, <addx, <addy, <pushz, <popz, <apply
syscall_vectors_h: .byte >calcscr, >setpattern, >vline, >hline, >frame, >box, >putstring, >putchar, >setzb, >setzw, >setzs, >addzb, >sbczb, >sbczbi, >addx, >addy, >pushz, >popz, >apply
syscall_args_l: .byte <args_calcscr, <args_setpattern, <args_vline, <args_hline, <args_frame, <args_box, <args_putstring, <args_putchar, <args_setzb, <args_setzw, <args_setzs, <args_addzb, <args_sbczb, <args_sbczbi, <args_addx, <args_addy, <args_pushz, <args_popz, <args_apply
syscall_args_h: .byte >args_calcscr, >args_setpattern, >args_vline, >args_hline, >args_frame, >args_box, >args_putstring, >args_putchar, >args_setzb, >args_setzw, >args_setzs, >args_addzb, >args_sbczb, >args_sbczbi, >args_addx, >args_addy, >args_pushz, >args_popz, >args_apply
