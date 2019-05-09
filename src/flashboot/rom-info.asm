.import main, ultifs_enter_root, ultifs_enter, ultifs_load

.segment "ROMINFO"

    ; ROM autostart info.
    .word main	; Cold start vector
    .word main	; Warm start vector
    .byte "A0", $c3, $c2, $cd	; "CBM"

    .word ultifs_enter_root
    .word ultifs_enter
    .word ultifs_load
