.export init_ultifs_dev

IOPEN   = $031A         ; KERNAL vector - open a logical file
ICLOSE  = $031C         ; KERNAL vector - close a specified logical file
ICHKIN  = $031E         ; KERNAL vector - open channel for input
ICKOUT  = $0320         ; KERNAL vector - open channel for output
ICLRCN  = $0322         ; KERNAL vector - close input and output channels
IBASIN  = $0324         ; KERNAL vector - input character from channel
IBSOUT  = $0326         ; KERNAL vector - output character to channel
ISTOP   = $0328         ; KERNAL vector - scan stop key
IGETIN  = $032A         ; KERNAL vector - get character from keyboard queue
ICLALL  = $032C         ; KERNAL vector - close all channels and files
USRCMD  = $032E         ; User vector
ILOAD   = $0330         ; KERNAL vector - load
ISAVE   = $0332         ; KERNAL vector - save

.data

old_vectors:    .res 13 * 2

.code

.proc init_ultifs_dev
    ldx #13 * 2 - 1
l:  lda $031a,x
    sta old_vectors,x
    dex
    bpl l
    rts
.endproc
