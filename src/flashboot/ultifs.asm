.export load
.importzp s, d, c

.segment "STARTUP"

journal_start = 65536                                                                                                       
blocks_start = 131072
files_start = 196608

block_record_size = 10
block_pos = 2
block_size = 6
block_next = 8

dirent_name = 2
dirent_type = 18
dirent_file = 19
dirent_size = 21

bp = 6

; Fetch byte from Flash memory at 24-bit offset 'bp'.
.proc get_byte
    lda $9ff2
    pha
    lda #%01111110
    sta $9ff2
    lda $9ff8
    pha
    lda $9ff9
    pha

    lda bp+1
    lsr
    lsr
    lsr
    lsr
    lsr
    sta $9ff8
    lda bp+2
    asl
    asl
    asl
    asl
    asl
    ora $9ff8
    sta $9ff8
    lda bp+2
    lsr
    lsr
    lsr
    lsr
    lsr
    sta $9ff9
    lda bp
    sta s
    lda bp+1
    and #%00000111
    ora #$20
    sta s+1
    ldy #0
    lda (s),y
    tax

    pla
    sta $9ff9
    pla
    sta $9ff8
    pla
    sta $9ff2
    txa
    rts
.endproc

.proc inc_bp
    inc bp
    bne done
    inc bp+1
    bne done
    inc bp+2
done:
    rts
.endproc

block_record = blocks_start + block_record_size + block_pos

.proc load
    lda #block_record .and $ff
    sta bp
    lda #(block_record >> 8) .and $ff
    sta bp+1
    lda #(block_record >> 16) .and $ff
    sta bp+2
    jsr get_byte
    pha
    inc bp
    jsr get_byte
    pha
    inc bp
    jsr get_byte
    pha
    inc bp      ; Skip convenience byte.
    inc bp
    jsr get_byte
    sta c
    inc bp
    jsr get_byte
    sta c+1
    inc c+1
    pla
    sta bp+2
    pla
    sta bp+1
    pla
    sta bp

    lda #$00
    sta d
    lda #$20
    sta d+1
l:  jsr get_byte
    ldy #0
    sta (d),y
    jsr inc_bp
    dec c
    bne l
    dec c+1
    bne l

    rts
.endproc
