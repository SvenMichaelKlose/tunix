.export main
.exportzp s, d, c, tmp
.importzp bp

.import ultimem_read_byte
.import ultimem_write_byte
.import clrram, moveram, __PRGEND__

.zeropage

s:              .res 2
d:              .res 4
c:              .res 2
base:           .res 4
ptr:            .res 4
size:           .res 4
next:           .res 4
replacement:    .res 4
tmp:            .res 1

.segment "STARTUP"

block_size = 0
block_next = 4
block_replacement = 8
block_namelen = 13

.proc main
    ; Don't get interrupted.
    sei
    lda #$7f
    sta $911d
    sta $911e

    cld
    ldx #$ff
    txs

    lda #$22    ; Red screen.
    sta $900f

    jsr $fd8d   ; Init memory.
    jsr $fd52   ; Init KERNAL.
    jsr $fdf9   ; Init VIAs.
    jsr $e518   ; Init hardware.

    lda #$7f    ; Yellow screen.
    sta $900f

    ; Activate all RAM.
    lda #%00111111
    sta $9ff1
    lda #%01111111
    sta $9ff2
    lda #0
    ldx #1
    stx $9ff4
    sta $9ff5
    inx
    stx $9ff6
    sta $9ff7
    inx
    stx $9ff8
    sta $9ff9
    inx
    stx $9ffa
    sta $9ffb
    inx
    stx $9ffc
    sta $9ffd
    cli

    ; Make dummy call to g's link().
    lda #$60	; RTS
    sta $0400

    ; Make pointer to start of file system.
    lda #$00
    ldx #$01
    sta base
    sta base+1
    stx base+2
    sta base+3

next_block:
    ldx #base
    ldy #ptr
    jsr copyd
    ldx #ptr
    ldy #size
    jsr read_int
    ldy #size
    jsr is_empty
    beq boot_not_found

    lda #block_replacement
    ldx #base
    ldy #ptr
    jsr add_ofs
    ldx #ptr
    ldy #replacement
    jsr read_int
    ldy #replacement
    jsr is_empty
    beq check_name

    ldx #ptr
    ldy #base
    jsr copyd
    jmp next_block

check_name:
    lda #block_namelen
    ldx #base
    ldy #ptr
    jsr add_ofs
    ldx #ptr
    jsr ultimem_read_byte
    cmp #bootfile_end - bootfile
    beq found
 
    lda #block_next
    ldx #base
    ldy #ptr
    jsr add_ofs
    ldx #ptr
    ldy #next
    jsr read_int
    ldy #next
    jsr is_empty
    beq boot_not_found

    ldx #ptr
    ldy #base
    jsr copyd
next_block2:
    jmp next_block
   
boot_not_found:
    jmp boot_not_found

found:
    ldx #ptr
    jsr inczpd
    ldy #0
l2: jsr ultimem_read_byte
    cmp bootfile,y
    bne next_block2
    jsr inczpd
    iny
    cpy #bootfile_end - bootfile
    bne l2

    lda #$fe
    sta d
    lda #$5f
    sta d+1
    lda #$00
    sta d+2
    sta d+3

    ; Load data
    lda #4
    ldx #ptr
    ldy #base
    jsr add_ofs
    inc size+1
l3: ldx #base
    jsr ultimem_read_byte
    jsr inczpd
    ldx #d
    jsr ultimem_write_byte
    jsr inczpd
    dec size
    bne l3
    dec size+1
    bne l3

    ; Run it.
    jmp $2000

.endproc

.proc dummy_link
    rts
.endproc

.proc copyd
    lda 0,x
    sta 0,y
    lda 1,x
    sta 1,y
    lda 2,x
    sta 2,y
    lda 3,x
    sta 3,y
    rts
.endproc

.proc inczpd
    inc 0,x
    bne n
    inc 1,x
    bne n
    inc 2,x
    bne n
    inc 3,x
n:  rts
.endproc

.proc add_ofs
    sta 0,y
    lda #0
    sta 1,y
    sta 2,y
    sta 3,y

    lda 0,x
    clc
    adc 0,y
    sta 0,y
    lda 1,x
    adc 1,y
    sta 1,y
    lda 2,x
    adc 2,y
    sta 2,y
    lda 3,x
    adc 3,y
    sta 3,y
    rts
.endproc

.proc read_int
    jsr ultimem_read_byte
    jsr inczpd
    sta 0,y
    jsr ultimem_read_byte
    jsr inczpd
    sta 1,y
    jsr ultimem_read_byte
    jsr inczpd
    sta 2,y
    jsr ultimem_read_byte
    jsr inczpd
    sta 3,y
    rts
.endproc

.proc is_empty
    lda 0,y
    and 1,y
    and 2,y
    and 3,y
    cmp #$ff
    rts
.endproc

bootfile:
    .byte "boot"
bootfile_end:
