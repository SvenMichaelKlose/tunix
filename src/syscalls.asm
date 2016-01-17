c_hline         = 1
c_vline         = 2
c_frame         = 3
c_box           = 4
c_putstring     = 5
c_setpattern    = 6
c_apply         = 7
c_addx          = 8
c_addy          = 9
c_setzb         = 10
c_setzw         = 11
c_addzb         = 12
c_sbczb         = 13
c_sbczbi        = 14

dummy:
    rts

syscall_vectors_l:
    <hline
    <vline
    <frame
    <box
    <putstring
    <dummy
    <apply
    <addx
    <addy
    <setzb
    <setzw
    <addzb
    <sbczb
    <sbczbi

syscall_vectors_h:
    >hline
    >vline
    >frame
    >box
    >putstring
    >dummy
    >apply
    >addx
    >addy
    >setzb
    >setzw
    >addzb
    >sbczb
    >sbczbi

syscall_args_l:
    <args_hline
    <args_vline
    <args_frame
    <args_box
    <args_putstring
    <args_setpattern
    <args_apply
    <args_addx
    <args_addy
    <args_setzb
    <args_setzw
    <args_addzb
    <args_sbczb
    <args_sbczbi

syscall_args_h:
    >args_hline
    >args_vline
    >args_frame
    >args_box
    >args_putstring
    >args_setpattern
    >args_apply
    >args_addx
    >args_addy
    >args_setzb
    >args_setzw
    >args_addzb
    >args_sbczb
    >args_sbczbi

args_hline:
    3
    xpos
    ypos
    width

args_vline:
    3
    xpos
    ypos
    height

args_frame:
args_box:
    4
    xpos
    ypos
    width
    height

args_putstring:
    4
    xpos
    ypos
    p
    @(++ p)

args_setpattern:
    2
    pattern
    @(++ pattern)

args_apply:
    0

args_addx:
args_addy:
    1
    tmp

args_setzb:
args_sbczbi:
    2
    tmp
    tmp2

args_setzw:
args_addzb:
args_sbczb:
    3
    tmp
    tmp2
    tmp3
