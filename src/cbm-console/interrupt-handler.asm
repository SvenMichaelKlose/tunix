.import _iopage, _active, _consoles, _menu_pid
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

    .code

SHFLAG  = $028d

; Keep relocatable for I/O page.
.export interrupt_handler
.proc interrupt_handler
    lda SHFLAG
    cmp #1  ; C= + SHIFT
    bne interrupt_handler3
    beq interrupt_handler3
    lda $9ff8
    pha
.endproc
.proc interrupt_handler2
    lda #0
    sta $9ff8
    ldx _active
    lda _consoles,x
    jsr lib_suspend
    lda _menu_pid
    jsr lib_resume
    pla
    sta $9ff8
.endproc
.proc interrupt_handler3
    jmp $ffff
.endproc

    .code

.export _install_interrupt_handler
.proc _install_interrupt_handler
    ; Configure handler.
    lda $9ff8
    sta interrupt_handler2 + 1
    lda $0314
    sta interrupt_handler3 + 1
    lda $0315
    sta interrupt_handler3 + 2

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

    ; Redirect IRQ.
    sei
    lda #0
    sta $0314
    lda _iopage
    sta $0315
    cli
    rts
.endproc
