main:
    ldy #@(- cinfo_kernal_end cinfo 1)
    jsr copy_backwards
    jmp reset

copy_cinfo:
    ldx #@(- cinfo_end cinfo 1)
l:  lda cinfo,y
    sta s,x
    dey
    dex
    bpl -l
    rts

copy_backwards:
    jsr copy_cinfo
    ldy #0
l:  lda (s),y
    sta (d),y
    dec s
    lda s
    cmp #$ff
    bne +n
    dec @(++ s)
n:  dec d
    lda d
    cmp #$ff
    bne +n
    dec @(++ d)
n:  dec c
    bne -l
    dec @(++ c)
    bne -l
    rts

kernal_size         = @(- kernal_end kernal)
loaded_kernal_end   = @(+ loaded_kernal (-- kernal_size))
kernal_end2         = @(-- kernal_end)

cinfo:
cinfo_kernal:
    <loaded_kernal_end >loaded_kernal_end
    <kernal_end2 >kernal_end2
    <kernal_size @(++ >kernal_size)
cinfo_kernal_end:
cinfo_end:

loaded_kernal:
    org $1a00

kernal:
reset:
    jsr $e5c3   ; INITVIC
    jsr gfx_init
w:  jmp -w

gfx_init:
    lda #@(+ 128 22)
    sta $9002
    lda #$ec    ; screen=$1800, chars=$1000
    sta $9005
    lda #@(+ (* white 16) reverse white)
    sta $900f

    lda #0
    tax
l:  sta $1000,x
    sta $1100,x
    sta $1200,x
    sta $1300,x
    sta $1400,x
    sta $1500,x
    sta $1600,x
    sta $1700,x
    dex
    bne -l
    rts
kernal_end:
