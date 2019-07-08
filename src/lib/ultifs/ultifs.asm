.export ultifs_enter_root
.export ultifs_enter
.export ultifs_load
.export copyd
.exportzp name, namelen

.import ultimem_read_byte
.import ultimem_write_byte
.import ultimem_get_bank
.import ultimem_copy_rom2ram
.importzp s, d, c
.importzp base, ptr, size

.zeropage

;base:           .res 4
;ptr:            .res 4
;size:           .res 4
next:           .res 4
replacement:    .res 4
namelen:        .res 1
name:           .res 2
ultifs_base:    .res 4

.code

block_size = 0
block_replacement = 4
block_next = 8
block_type = 12
block_namelen = 13

.proc ultifs_enter_root
    lda #$00
    ldx #$01
    sta ultifs_base
    sta ultifs_base+1
    stx ultifs_base+2
    sta ultifs_base+3
    rts
.endproc

    ; Enter directory.
.proc ultifs_enter
    jsr ultifs_find
    bcc n
    ldy #ultifs_base
    jsr ultifs_read_int
    sec
n:  rts
.endproc

.proc ultifs_load
    jsr ultifs_find
    bcs l
    rts
l:  ldx #ptr
    ldy #s
    jsr copyd
    jmp ultimem_copy_rom2ram
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
    jsr ultifs_read_int
    ldy #size
    jsr is_empty
    beq boot_not_found

    lda #block_replacement
    ldx #base
    ldy #ptr
    jsr add_ofs
    ldx #ptr
    ldy #replacement
    jsr ultifs_read_int
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
 
next_block2:
    lda #block_next
    ldx #base
    ldy #ptr
    jsr add_ofs
    ldx #ptr
    ldy #next
    jsr ultifs_read_int
    ldy #next
    jsr is_empty
    beq boot_not_found

    ldx #next
    ldy #base
    jsr copyd
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

.proc ultifs_read_int
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
