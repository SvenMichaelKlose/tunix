reset:
    jsr $e5c3   ; INITVIC
    jsr gfx_init
w:  jmp -w
