main:
    ldx #@(- cinfo_end cinfo 1)
l:  lda cinfo,x
    sta s,x
    dex
    bpl -l

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

    jmp reset

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
