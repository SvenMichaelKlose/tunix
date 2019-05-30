.import main, save_state

.segment "ROMINFO"

    ; ROM autostart info.
    .word main	; Cold start vector
    .word main	; Warm start vector
    .byte "A0", $c3, $c2, $cd	; "CBM"

    .word save_state
