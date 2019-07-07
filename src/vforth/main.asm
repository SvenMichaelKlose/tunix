.export _main

.importzp s, d, c
.import moveram
.import binary, binary_end

.code

size = binary_end - binary
real_end = $2000 + size

.proc _main
    lda #<binary_end ;(binary_end-1)
    sta s
    lda #>binary_end ;(binary_end-1)
    sta s+1
    lda #<(real_end-2)
    sta d
    lda #>(real_end-2)
    sta d+1
    lda #<size
    sta c
    lda #>size
    sta c+1
    lda #1
    jsr moveram
    jmp $2000
.endproc
