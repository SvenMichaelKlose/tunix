.export main
.exportzp s, d, c, tmp
.importzp bp

.import ultimem_read_byte
.import ultimem_write_byte
.import ultimem_get_bank
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
namelen:        .res 1
name:           .res 2
ultifs_base:    .res 4

.segment "STARTUP"

block_size = 0
block_replacement = 4
block_next = 8
block_type = 12
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
    lda #$00    ; Blank screen.
    sta $9002

    jsr $fd8d   ; Init memory.
    jsr $fd52   ; Init KERNAL.
    jsr $fdf9   ; Init VIAs.
    jsr $e518   ; Init hardware.

    lda #$7f    ; Yellow screen.
    sta $900f

    jsr init_ram_banks

    cli

    ; Set root directory.
    lda #$00
    ldx #$01
    sta ultifs_base
    sta ultifs_base+1
    stx ultifs_base+2
    sta ultifs_base+3

    lda #$00
    sta d
    lda #$40
    sta d+1
    lda #$00
    sta d+2
    sta d+3
    lda #<fn_boot
    sta name
    lda #>fn_boot
    sta name+1
    lda #fn_boot_end-fn_boot
    sta namelen
    jsr ultifs_load

    lda #$00
    sta d
    lda #$20
    sta d+1
    lda #$00
    sta d+2
    sta d+3
    lda #<fn_desktop
    sta name
    lda #>fn_desktop
    sta name+1
    lda #fn_desktop_end-fn_desktop
    sta namelen
    jsr ultifs_load

    lda #$00
    sta d
    lda #$80
    sta d+1
    lda #$00
    sta d+2
    sta d+3
    lda #<fn_ultifs
    sta name
    lda #>fn_ultifs
    sta name+1
    lda #fn_ultifs_end-fn_ultifs
    sta namelen
    jsr ultifs_load

    lda #$00
    sta d
    lda #$a0
    sta d+1
    lda #$00
    sta d+2
    sta d+3
    lda #<fn_charset4x8
    sta name
    lda #>fn_charset4x8
    sta name+1
    lda #fn_charset4x8_end-fn_charset4x8
    sta namelen
    jsr ultifs_load

    ; Run it.
    jmp $4000
.endproc

.proc ultifs_load
    jsr ultifs_find
n:  bcc n
    jmp ultifs_copy2ram
.endproc

.proc ultifs_find
    lda ultifs_base
    sta base
    lda ultifs_base+1
    sta base+1
    lda ultifs_base+2
    sta base+2
    lda ultifs_base+3
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

    ldx #replacement
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
    cmp namelen
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

    ldx #next
    ldy #base
    jsr copyd
next_block2:
    jmp next_block
   
boot_not_found:
    clc
    rts

found:
    ldx #ptr
    jsr inczpd
    ldy #0
l2: jsr ultimem_read_byte
    cmp (name),y
    bne next_block2
    jsr inczpd
    iny
    cpy namelen
    bne l2

    sec
    rts
.endproc

.proc ultifs_copy2ram
    lda #%01111101  ; ROMRAMRAMROM…
    sta $9ff2

    ldx #ptr
    ldy #base
    jsr copyd

    ldx #base
    ldy #$08
    jsr ultimem_get_bank
    lda s
    sta base
    lda s+1
    ora #$20
    sta base+1

    ldx #d
    ldy #$0a
    jsr ultimem_get_bank
    lda s
    sta d
    lda s+1
    ora #$40
    sta d+1

l3: ldy #0
    lda (base),y
    sta (d),y
    inc base
    bne l4
    inc base+1
    lda base+1
    cmp #$40
    bne l4
    lda #$20
    sta base+1
    inc $9ff8
    bne l4
    inc $9ff9

l4: inc d
    bne l5
    inc d+1
    lda d+1
    cmp #$60
    bne l5
    lda #$40
    sta d+1
    inc $9ffa
    bne l5
    inc $9ffb

l5: dec size
    lda size
    cmp #255
    bne l3
    dec size+1
    lda size+1
    cmp #255
    bne l3

    jmp init_ram_banks
.endproc

.proc dummy_link
    rts
.endproc

.proc init_ram_banks
    ; Activate all RAM.
    lda #%00111111
    sta $9ff1
    lda #%01111111
    sta $9ff2
    lda #0
    tax
    stx $9ff4
    sta $9ff5
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

fn_g:
    .byte "g"
fn_g_end:

fn_boot:
    .byte "boot"
fn_boot_end:

fn_desktop:
    .byte "desktop"
fn_desktop_end:

fn_ultifs:
    .byte "ultifs"
fn_ultifs_end:

fn_charset4x8:
    .byte "charset-4x8"
fn_charset4x8_end:
