.export init_region_stack, push_context, pop_context, add_region_position, sub_region_position

.importzp xpos, ypos, rxl, ryt, region_sp
.importzp context_start, context_size

.code

.proc init_region_stack
    lda #<region_stack
    sta region_sp
    lda #>region_stack
    sta region_sp+1
    rts
.endproc

.proc push_context
    ldy #context_size-1
l:  lda context_start,y
    sta (region_sp),y
    dey
    bpl l

    lda region_sp
    clc
    adc #4
    sta region_sp
    bcc n
    inc region_sp+1
n:

    rts
.endproc

.proc pop_context
    lda region_sp
    sec
    sbc #4
    sta region_sp
    bcs n
    dec region_sp+1
n:

    ldy #context_size-1
l:  lda (region_sp),y
    sta context_start,y
    dey
    bpl l

    rts
.endproc

.proc add_region_position
    lda rxl
    clc
    adc xpos
    sta xpos

    lda ryt
    clc
    adc ypos
    sta ypos

    rts
.endproc

.proc sub_region_position
    lda xpos
    sec
    sbc rxl
    sta xpos

    lda ypos
    sec
    sbc ryt
    sta ypos

    rts
.endproc

.bss

region_stack:
    .res 256, 0
