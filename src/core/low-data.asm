;;; task-switching.asm

current_process = $2a1  ; Index into process tables in master core.
takeovers = $02a2
needs_switch = $02a3

;;; /dev/kbd

column_mask = $2a4
row_mask = $2a5

;;; /dev/con

xpos = $2a6
ypos = $2a7
xcpos = $2a8
