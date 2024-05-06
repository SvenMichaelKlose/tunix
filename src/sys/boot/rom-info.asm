.import __RAMIFY_RUN__

    .segment "ROMINFO"

; ROM autostart info.
.word __RAMIFY_RUN__    ; Cold start vector
.word __RAMIFY_RUN__    ; Warm start vector
.byte "A0", $c3, $c2, $cd	; "CBM"
