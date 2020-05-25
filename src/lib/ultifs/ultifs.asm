.export ultifs_enter_root
.export ultifs_enter
.export ultifs_load
.exportzp name, namelen

.import ultimem_read_byte
.import ultimem_write_byte
.import ultimem_get_bank
.import ultimem_copy_rom2ram
.importzp s, d, c
.importzp base, ptr, size

.import zpd_addb_xay
.import zpd_mov_xy
.import zpd_inc_x
.import zpd_is_minus1_y

.zeropage

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
    jsr zpd_mov_xy
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
    jsr zpd_mov_xy
    ldx #ptr
    ldy #size
    jsr ultifs_read_int
    ldy #size
    jsr zpd_is_minus1_y
    beq boot_not_found

    lda #block_replacement
    ldx #base
    ldy #ptr
    jsr zpd_addb_xay
    ldx #ptr
    ldy #replacement
    jsr ultifs_read_int
    ldy #replacement
    jsr zpd_is_minus1_y
    beq check_name

    ldx #replacement
    ldy #base
    jsr zpd_mov_xy
    jmp next_block

check_name:
    lda #block_namelen
    ldx #base
    ldy #ptr
    jsr zpd_addb_xay
    ldx #ptr
    jsr ultimem_read_byte
    cmp namelen
    beq found
 
next_block2:
    lda #block_next
    ldx #base
    ldy #ptr
    jsr zpd_addb_xay
    ldx #ptr
    ldy #next
    jsr ultifs_read_int
    ldy #next
    jsr zpd_is_minus1_y
    beq boot_not_found

    ldx #next
    ldy #base
    jsr zpd_mov_xy
    jmp next_block
   
boot_not_found:
    clc
    rts

found:
    ldx #ptr
    jsr zpd_inc_x
    ldy #0
l2: jsr ultimem_read_byte
    cmp (name),y
    bne next_block2
    jsr zpd_inc_x
    iny
    cpy namelen
    bne l2

    sec
    rts
.endproc

.proc ultifs_read_int
    jsr ultimem_read_byte
    jsr zpd_inc_x
    sta 0,y
    jsr ultimem_read_byte
    jsr zpd_inc_x
    sta 1,y
    jsr ultimem_read_byte
    jsr zpd_inc_x
    sta 2,y
    jsr ultimem_read_byte
    jsr zpd_inc_x
    sta 3,y
    rts
.endproc
