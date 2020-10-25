; Loads UltiFS into its bank and installs primary wedge
; for the current process.
;
; WANTED: The primary wedge is very small, so it fits
; into the unused low memory area from $02a1-$2ff.
; The user should be able to pick any position for it
; as long as it's not in BLK1-BLK3 ($2000-$7fff).
; Also the device number should be selectable.

.export _main

.forceimport __STARTUP__

.import init_secondary_wedge

IOPEN   = $031A     ; KERNAL vector - open a logical file
ICLOSE  = $031C     ; KERNAL vector - close a specified logical file
ICHKIN  = $031E     ; KERNAL vector - open channel for input
ICKOUT  = $0320     ; KERNAL vector - open channel for output
ICLRCN  = $0322     ; KERNAL vector - close input and output channels
IBASIN  = $0324     ; KERNAL vector - input character from channel
IBSOUT  = $0326     ; KERNAL vector - output character to channel
ICLALL  = $032C     ; KERNAL vector - close all channels and files
ILOAD   = $0330     ; KERNAL vector - load
ISAVE   = $0332     ; KERNAL vector - save

.data

txt_welcome:
    .byte "ULTIFS WEDGE", 13, 0

.code

.proc _fook
    rts
.endproc

.proc _main
    lda #<txt_welcome
    ldy #>txt_welcome
    jsr $cb1e
    jsr init_secondary_wedge

    rts
.endproc
