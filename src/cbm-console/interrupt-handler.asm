.import _iopage, _active, _menu_pid
.import lib_suspend, lib_resume
.import moveram

.exportzp s, d, c

    .zeropage

s:
sl: .res 1
sh: .res 1
d:
dl: .res 1
dh: .res 1
c:
cl: .res 1
ch: .res 1

    .bss

old_handler:    .res 2

    .code

SHFLAG  = $028d

; Keep relocatable for I/O page.
.proc interrupt_handler
    lda SHFLAG
    cmp #3  ; C= + SHIFT
    bne done
    lda _active
    jsr lib_suspend
    lda _menu_pid
    jsr lib_resume
done:
    jmp (old_handler)
.endproc

.export _install_interrupt_handler
.proc _install_interrupt_handler
    ; Move interrupt handler to I/O page.
    lda #<interrupt_handler
    sta sl
    lda #>interrupt_handler
    sta sh
    lda _iopage
    sta dh
    lda #1
    sta ch
    lda #0
    sta dl
    sta cl
    jsr moveram

    ; Install handler.
    sei
    lda $0314
    sta old_handler
    lda $0315
    sta old_handler+1
    lda #0
    sta $0314
    lda _iopage
    sta $0315
    cli
    rts
.endproc
