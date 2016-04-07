;;; task-switching.asm

takeovers = $02a1
last_error = $2a2
needs_switch = $02a3

;;; /dev/kbd

column_mask = $2a4
row_mask = $2a5

;;; /dev/con

xpos = $2a6
ypos = $2a7
xcpos = $2a8
