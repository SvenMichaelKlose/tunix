    ; Open the library.
    ; Allocate and populate +3K area.
    ; Load index into upper half of +3K area.
    ; Load rest of library like a regular program, so it gets linked.
    ; Make jump table for caller.
    rts
