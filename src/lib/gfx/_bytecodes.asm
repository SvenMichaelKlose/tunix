.importzp d, tmp, tmp2, tmp3, ph, p, width, height, patternh, pattern, xpos, ypos
.import calcscr, setpattern, vline, hline, frame, box, putstring, putchar, get_text_width, setzb, setzw, setzs, addzb, sbczb, sbczbi, addx, addy, pushz, popz, apply
.importzp s
.data
c_calcscr=1
.export c_calcscr
c_setpattern=2
.export c_setpattern
c_vline=3
.export c_vline
c_hline=4
.export c_hline
c_frame=5
.export c_frame
c_box=6
.export c_box
c_putstring=7
.export c_putstring
c_putchar=8
.export c_putchar
c_get_text_width=9
.export c_get_text_width
c_setzb=10
.export c_setzb
c_setzw=11
.export c_setzw
c_setzs=12
.export c_setzs
c_addzb=13
.export c_addzb
c_sbczb=14
.export c_sbczb
c_sbczbi=15
.export c_sbczbi
c_addx=16
.export c_addx
c_addy=17
.export c_addy
c_pushz=18
.export c_pushz
c_popz=19
.export c_popz
c_apply=20
.export c_apply
.export syscall_vectors_l
syscall_vectors_l:  .byte <calcscr, <setpattern, <vline, <hline, <frame, <box, <putstring, <putchar, <get_text_width, <setzb, <setzw, <setzs, <addzb, <sbczb, <sbczbi, <addx, <addy, <pushz, <popz, <apply
.export syscall_vectors_h
syscall_vectors_h:  .byte >calcscr, >setpattern, >vline, >hline, >frame, >box, >putstring, >putchar, >get_text_width, >setzb, >setzw, >setzs, >addzb, >sbczb, >sbczbi, >addx, >addy, >pushz, >popz, >apply
.export syscall_args_l
syscall_args_l:  .byte <args_calcscr, <args_setpattern, <args_vline, <args_hline, <args_frame, <args_box, <args_putstring, <args_putchar, <args_get_text_width, <args_setzb, <args_setzw, <args_setzs, <args_addzb, <args_sbczb, <args_sbczbi, <args_addx, <args_addy, <args_pushz, <args_popz, <args_apply
.export syscall_args_h
syscall_args_h:  .byte >args_calcscr, >args_setpattern, >args_vline, >args_hline, >args_frame, >args_box, >args_putstring, >args_putchar, >args_get_text_width, >args_setzb, >args_setzw, >args_setzs, >args_addzb, >args_sbczb, >args_sbczbi, >args_addx, >args_addy, >args_pushz, >args_popz, >args_apply
.export args_calcscr
args_calcscr: .byte 2, xpos, ypos
.export args_setpattern
args_setpattern: .byte 2, pattern, pattern+1
.export args_vline
args_vline: .byte 3, xpos, ypos, height
.export args_hline
args_hline: .byte 3, xpos, ypos, width
.export args_frame
args_frame: .byte 4, xpos, ypos, width, height
.export args_box
args_box: .byte 4, xpos, ypos, width, height
.export args_putstring
args_putstring: .byte 2, p, ph
.export args_putchar
args_putchar: .byte 0
.export args_get_text_width
args_get_text_width: .byte 2, s, s+1
.export args_setzb
args_setzb: .byte 2, tmp, tmp2
.export args_setzw
args_setzw: .byte 3, tmp, tmp2, tmp3
.export args_setzs
args_setzs: .byte 2, d, tmp
.export args_addzb
args_addzb: .byte 3, tmp, tmp2, tmp3
.export args_sbczb
args_sbczb: .byte 3, tmp, tmp2, tmp3
.export args_sbczbi
args_sbczbi: .byte 2, tmp, tmp2
.export args_addx
args_addx: .byte 1, tmp
.export args_addy
args_addy: .byte 1, tmp
.export args_pushz
args_pushz: .byte 2, tmp, tmp2
.export args_popz
args_popz: .byte 2, tmp, tmp2
.export args_apply
args_apply: .byte 0
