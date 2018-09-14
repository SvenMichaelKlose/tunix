.export _cbm_opendir, _cbm_readdir, _cbm_closedir, _cbm_read_char

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
    ldy #$60    ; Read.
    lda #$02    ; Reserved logical file number.
    jsr SETLFS

    jsr OPEN
    bcs error

    ldx #$02
    jsr CHKIN
    bcs error

    ; Skip load address.
    jsr read
    bcs error
    jsr read
    bcs error

    ;; Skip first three lines.
    ldx #3
    ; Skip load address, first address of next line and line number.
m:  ldy #6
l1: jsr read
    bcs error
    dey
    bne l1

    ; Skip line.
    ldy #0
l2: jsr read
    bcs error
    bne l2     ; continue until end of line

    dex
    bne m

    lda #0
    tax
    rts
.endproc

; int __fastcall__ cbm_readdir (char * buffer)
.proc _cbm_readdir
    sta d
    stx d+1

    lda #0
    ldy #dirent_name
    sta (d),y

    jsr READST
    bne done

    ; Skip address of next line.
    jsr read
    bcs error2
    jsr read
    bcs error2

    ; Read BASIC line number, which is the size in blocks.
    jsr read
    bcs error2
    ldy #dirent_length
    sta (d),y
    jsr read
    bcs error2
    iny
    sta (d),y

    ; Read until first double quote.
l3: jsr read
    beq _cbm_readdir
    cmp #$22
    bne l3

    ; Read name till quote.
    ldy #dirent_name
l4: jsr read
    bcs error2
    beq _cbm_readdir
    cmp #$22
    beq n
    sta (d),y
    iny
    jmp l4
n:  lda #0
    sta (d),y

    ; Read till end of line.
l5: jsr read
    bne l5
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
