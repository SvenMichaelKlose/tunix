c_hline         = 1
c_vline         = 2
c_frame         = 3
c_box           = 4
c_putstring     = 5
c_setpattern    = 6
c_apply         = 7

syscall_vectors_l:
    <hline
    <vline
    <frame
    <box
    <putstring
    <dummy
    <apply

syscall_vectors_h:
    >hline
    >vline
    >frame
    >box
    >putstring
    >dummy
    >apply

syscall_args_l:
    <args_hline
    <args_vline
    <args_frame
    <args_box
    <args_putstring
    <args_setpattern
    <args_apply

syscall_args_h:
    >args_hline
    >args_vline
    >args_frame
    >args_box
    >args_putstring
    >args_setpattern
    >args_apply

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

dummy:
    rts
