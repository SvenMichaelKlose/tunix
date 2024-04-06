.import __STARTUP_RUN__

    .segment "ROMINFO"

; ROM autostart info.
.word __STARTUP_RUN__	; Cold start vector
.word __STARTUP_RUN__   ; Warm start vector
.byte "A0", $c3, $c2, $cd	; "CBM"
