.export _ultimem_copy_rom2ram

.import ultimem_copy_rom2ram
.import popax
.importzp s, d, c

.proc _ultimem_copy_rom2ram
    sta c
    stx c+1
    jsr popax

    sta d+2
    stx d+3
    jsr popax
    sta d
    stx d+1
    jsr popax

    sta s+2
    stx s+3
    jsr popax
    sta s
    stx s+1

    jmp ultimem_copy_rom2ram
.endproc
