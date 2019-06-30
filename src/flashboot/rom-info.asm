.import main, save_state, launch

.segment "ROMINFO"

    ; ROM autostart info.
    .word main	; Cold start vector
    .word main	; Warm start vector
    .byte "A0", $c3, $c2, $cd	; "CBM"

    jmp save_state
    jmp launch
