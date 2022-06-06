.import main, save_state, launch, alloc_bank, free_bank, copy_bank, ingle_exec

.segment "ROMINFO"

    ; ROM autostart info.
    .word main	; Cold start vector
    .word main	; Warm start vector
    .byte "A0", $c3, $c2, $cd	; "CBM"
