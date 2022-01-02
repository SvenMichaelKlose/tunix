.export _reset

.proc _reset
    sei
    jsr $e5c3   ; INITVIC
    jmp $e378   ; BASIC cold start
.endproc
