.export ultimem_copy_rom2ram
.export ultimem_copy_ram2rom
.export ultimem_copy_ram2ram
.importzp s, d, c, base, size, ptr

.import ultimem_get_bank

sreg = $9ff8
dreg = $9ffa
sregidx = sreg - $9ff0
dregidx = dreg - $9ff0
sofs = $2000
dofs = $4000

.code

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

.proc ultimem_copy
    lda sreg
    pha
    lda sreg+1
    pha
    lda dreg
    pha
    lda dreg+1
    pha

    ldx #ptr
    ldy #base
    jsr copyd

    ldx #base
    ldy #sregidx
    jsr ultimem_get_bank
    lda s
    sta base
    lda s+1
    ora #>sofs
    sta base+1

    ldx #d
    ldy #dregidx
    jsr ultimem_get_bank
    lda s
    sta d
    lda s+1
    ora #>dofs
    sta d+1

l3: ldy #0
    lda (base),y
    sta (d),y
    inc base
    beq inc_base

l4: inc d
    beq inc_d

l5: dec size
    lda size
    cmp #255
    bne l3
    dec size+1
    lda size+1
    cmp #255
    bne l3

    pla
    sta dreg+1
    pla
    sta dreg
    pla
    sta sreg+1
    pla
    sta sreg
    pla
    sta $9ff2
    rts

inc_base:
    inc base+1
    lda base+1
    cmp #>sofs+$20
    bne l4
    lda #>sofs
    sta base+1
    inc sreg
    bne l4
    inc sreg+1
    jmp l4

inc_d:
    inc d+1
    lda d+1
    cmp #>dofs+$20
    bne l5
    lda #>dofs
    sta d+1
    inc dreg
    bne l5
    inc dreg+1
    jmp l5
.endproc

.proc ultimem_copy_rom2ram
    lda $9ff2
    pha

    lda #%01111101  ; ROMROMRAMROM…
    sta $9ff2
    jmp ultimem_copy
.endproc

.proc ultimem_copy_ram2rom
    lda $9ff2
    pha

    lda #%01110111  ; ROMRAMROMROM…
    sta $9ff2
    jmp ultimem_copy
.endproc

.proc ultimem_copy_ram2ram
    lda $9ff2
    pha

    lda #%01111111  ; ROMRAMRAMRAM…
    sta $9ff2
    jmp ultimem_copy
.endproc
