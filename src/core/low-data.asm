found_memory_expansion = $80

current_process = $2a1  ; Index into process tables in master core.
takeovers = $02a2
needs_switch = $02a3
max_banks = $02a4
malloc_bank = $02a5
old_nmi = $02a6

;;; /dev/con

xpos = $2a8
ypos = $2a9
xcpos = $2aa
devcon_mode = $2ab
devcon_cursor_x = $2ac
devcon_cursor_y = $2ad
