.export init_ultifs_dev

FNLEN   = $B7       ; file name length

LA  = $B8           ; logical file
SA  = $B9           ; secondary address
FA  = $BA           ; current device number
                    ; number    device
                    ; ------    ------
                    ;  0        keyboard
                    ;  1        cassette
                    ;  2        RS-232
                    ;  3        screen
                    ;  4-31     serial bus
FNADR   = $BB       ; file name pointer low byte

IOPEN   = $031A     ; KERNAL vector - open a logical file
ICLOSE  = $031C     ; KERNAL vector - close a specified logical file
ICHKIN  = $031E     ; KERNAL vector - open channel for input
ICKOUT  = $0320     ; KERNAL vector - open channel for output
ICLRCN  = $0322     ; KERNAL vector - close input and output channels
IBASIN  = $0324     ; KERNAL vector - input character from channel
IBSOUT  = $0326     ; KERNAL vector - output character to channel
ISTOP   = $0328     ; KERNAL vector - scan stop key
IGETIN  = $032A     ; KERNAL vector - get character from keyboard queue
ICLALL  = $032C     ; KERNAL vector - close all channels and files
USRCMD  = $032E     ; User vector
ILOAD   = $0330     ; KERNAL vector - load
ISAVE   = $0332     ; KERNAL vector - save

.bss

new_vectors:
    .word open
    .word close
    .word chkin
    .word chkout
    .word clrcn
    .word basin
    .word basout
    .word stop
    .word getin
    .word clall
    .word usrcmd
    .word load
    .word save

.data

old_vectors:
uopen:      .res 2
uclose:     .res 2
uchkin:     .res 2
uchkout:    .res 2
uclrcn:     .res 2
ubasin:     .res 2
ubasout:    .res 2
ustop:      .res 2
ugetin:     .res 2
uclall:     .res 2
uusrcmd:    .res 2
uload:      .res 2
usave:      .res 2

.code

.proc init_ultifs_dev
    ldx #13 * 2 - 1
l:  lda $031a,x
    sta old_vectors,x
    lda new_vectors,x
    sta $031a,x
    dex
    bpl l

    rts
.endproc

.proc open
    lda FA
    cmp #9
    beq found_dev
    jmp (uopen)
found_dev:

    ; Init directory output.
.endproc

.proc close
    jmp (uclose)
    ; Check if file was opened.
.endproc

.proc chkin
    jmp (uchkin)
.endproc

.proc chkout
    jmp (uchkout)
.endproc

.proc clrcn
    jmp (uclrcn)
    ; Do nothing.
.endproc

.proc basin
    jmp (ubasin)
.endproc

.proc basout
    jmp (ubasin)
.endproc

.proc stop
    jmp (ustop)
.endproc

.proc getin
    lda FA
    beq keyboard
    jmp (ugetin)
keyboard:
    jmp basin
.endproc

.proc clall
    jmp (uclall)
    ; Do nothing.
.endproc

.proc usrcmd
    jmp (uusrcmd)
.endproc

.proc load
    jmp (uload)
    ; Get source bank.
.endproc

.proc save
    jmp (usave)
    ; Get destination bank.
.endproc
