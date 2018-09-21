.export _cbm_opendir, _cbm_readdir, _cbm_closedir, _cbm_read_char
.export _cbm_readst = READST

.importzp s, d
.import popax

.include "../core/dev/cbm/kernal.asm"

max_file_name_length = 16
dirent_name     = 0
dirent_length   = max_file_name_length
dirent_type     = dirent_length + 4
dirent_size     = dirent_type + 1

.proc error
    ldx #1
    rts
.endproc

; int __fastcall__ cbm_opendir (char * pathname, char device);
.proc _cbm_opendir
    pha
    jsr popax
    sta s
    stx s+1
    jsr strlen
    tya
    ldx s
    ldy s+1
    jsr SETNAM

    pla
    tax
    ldy #$00    ; Read.
    lda #$02    ; Reserved logical file number.
    jsr SETLFS

    jsr OPEN
    bcs error

    ldx #$02
    jsr CHKIN
    bcs error

    ; Skip pointer to next sector.
    jsr read
    bcs error
    jsr read
    bcs error

    lda #0
    tax
    rts
.endproc

; int __fastcall__ cbm_readdir (char * buffer)
.proc _cbm_readdir
    sta d
    stx d+1

    lda #0
    tay
    sta (d),y

    jsr READST
    bne done

    ldx #31
    ldy #0
l:  jsr read
    bcs error2
    sta (d),y
    iny
    dex
    bne l

done:
    lda #0
    tax
    rts

error2:
    jsr READST
    bne done
    lda #255
    tax
    rts
.endproc

; int __fastcall__ cbm_closedir ();
.proc _cbm_closedir
    lda #$02
    jsr CLOSE
    bcs e
    jsr CLRCHN

    lda #0
    tax
    rts

e:  jmp error
.endproc

.proc read
    jsr READST
    bne eof

    jsr CHRIN
    bcs e
    pha
    lda $90
    cmp #1   ; set carry when ST>0 (i.e., <>0!)
    pla      ; keep carry, and possibly set Z flag for byte=0
    rts

eof:
    clc
    rts

e:  jmp error
.endproc

; s: String
;
; Returns:
; Y: length, excluding terminating zero.
.proc strlen
    pha
    ldy #0
l:  lda (s),y
    beq n
    iny
    jmp l
n:  pla
    rts
.endproc

.proc _cbm_read_char
    jsr GETIN
    ldx #0
    rts
.endproc
