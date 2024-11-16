; Tasks:
;
; * Cast return flags if wanted.
; * Branch inversion, instead of flag inversion.
; * Turn register arg snippets into procedures.
; * Move stack place between temporary to match IGEN definition args.
;   (Look up %CAST?)
; * INCSP/PUSHXA -> STSPAX
; * INCSP/STSPAX -> PUSHAX
; * DECSP/LDSPAX -> POPAX
; * Optimize sp+0.
; * Use registers instead of stack place 0.
; * Remove overwritten stores.

;;;;;;;;;;;;;;;;;;;;;
;;; STACK POINTER ;;;
;;;;;;;;;;;;;;;;;;;;;
;
; Word-aligned!
;
; The stack pointer consists of a pointer with the low byte set to
; 0 and a separate low byte index value for the Y register.
;
; spl:  .res 1
; sp:   .res 1
; sph:  .res 1

(igen incsp nil ()
    ldy spl
    iny
    iny
    bne l
    inc sph
l:  sty spl)

(igen decsp nil ()
    ldy spl
    bne l
    dec sph
l:  dey
    dey
    sty spl)

(igen addsp nil (# i)
    lda spl
    clc
    adc #i
    sta spl
    bcc l
    inc sph
l:)

(igen subsp nil (# i)
    lda spl
    sec
    sbc #i
    sta spl
    bcs l
    dec sph
l:)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PUSH/POP OBJECT STACK ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(igen push nil (ax)
    ldy spl
    bne l
    dec sph
l:  dey
    dey
    sta (sp),y
    txa
    iny
    sta (sp),y
    dey
    sty spl)

(igen pop ax ()
    ldy spl
    iny
    lda (sp),y
    tax
    dey
    lda (sp),y
    iny
    iny
    bne l
    inc sph
l:  sty spl)

(igen push nil (x)
    ldy spl
    bne l
    dec sph
l:  dey
    lda x
    sta (sp),y
    lda (++ x)
    dey
    sta (sp),y
    sty spl)

(igen pop x ()
    ldy spl
    lda (sp),y
    sta (++ x)
    iny
    lda (sp),y
    sta x
    iny
    bne l
    inc sph
l:  sty spl)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STACK PLACE ACCESS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(igen ldsp ax ((# i))
    lda spl
    sta sp
    ldy #(++ i)
    lda (sp),y
    tax
    dey
    lda (sp),y
    ldy #0
    sty sp)

(igen stsp nil ((# i))
    ldy spl
    sty sp
    ldy #i
    sta (sp),y
    iny
    txa
    sta (sp),y
    ldy #0
    sty sp)

;;;;;;;;;;;;;;;;;;
;;; LOAD/STORE ;;;
;;;;;;;;;;;;;;;;;;

(igen ldax ((# x))
    ldx #<x
    lda #>x)

(igen ldax (x)
    ldx x
    lda (++ x))

(igen stax ax (x ax)
    stx x
    sta (++ x))

(igen = ax (x ax)
  (stax ax))

;;;;;;;;;;;;;;
;;; CONSES ;;;
;;;;;;;;;;;;;;

(igen lcar ax (x)
    ldy #0
    lsr
    bcc l
    lda #<nil
    ldx #>nil
    beq n
l:  lda (x),y
    ldy #(++ cons_car)
    lda (x),y
    tax
    dey
    lda (x),y
n:)

(igen car ax (x)
    ldy #(++ cons_car)
    lda (x),y
    tax
    dey
    lda (x),y)

(igen cdr ax (x)
    ldy #(++ cons_cdr)
    lda (x),y
    tax
    dey
    lda (x),y)

(igen =car (x ax)
    ldy #cons_car
    sta (x),y
    txa
    iny
    sta (x),y)

(igen =cdr (x ax)
    ldy #cons_cdr
    sta (x),y
    txa
    iny
    sta (x),y)

;;;;;;;;;;;;;
;;; JUMPS ;;;
;;;;;;;;;;;;;

(igen go (l)
    jmp l)

(igen go-if (z l)
    beq l)

(igen go-if (nz l)
    bne l)

(igen go-if (c l)
    bcs l)

(igen go-if (nc l)
    bcc l)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FLAG TO BOOL CASTS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
; Required for predicates.

(igen %cast ax (z)
    beq l
    (sett ax)
    jmp l2
l:  (setnil ax)
l2:)

(igen %cast ax (nz)
    bne l
    (sett ax)
    jmp l2
l:  (setnil ax)
l2:)

(igen %cast ax (c)
    bcc l
    (sett ax)
    jmp l2
l:  (setnil ax)
l2:)

(igen %cast ax (nc)
    bcs l
    (sett ax)
    jmp l2
l:  (setnil ax)
l2:)

;;;;;;;;;;;;;;;;;;
;;; PREDICATES ;;;
;;;;;;;;;;;;;;;;;;

(igen not z (a)
    cmp #0)

(igen not z (ax)
    cpx #0
    bne n
    cmp #0
n:)

(igen not z ((byte x))
    lda x)

(igen not z ((word x))
    lda x
    ora (++ x))

(igen not z (x)
    lda (++ x))

(igen atom z (x)
    ldy #0
    lda (x),y
    and #TYPE_CONS)

(igen atom nc (x)
    ldy #0
    lda (x),y
    lsr)

(igen cons? c (x)
    ldy #0
    lda (x),y
    lsr)

(igen ccons? z (x)
    ldy #0
    lda (x),y
    and #(bit-or TYPE_CONS TYPE_EXTENDED)
    cmp #(bit-or TYPE_CONS TYPE_EXTENDED))

(igen symbol? nz (x)
    ldy #0
    lda (x),y
    and #TYPE_SYMBOL)

(igen number? nz (x)
    ldy #0
    lda (x),y
    and #TYPE_NUMBER)

;;;;;;;;;;;;;;;;;;;
;;; ARITHMETICS ;;;
;;;;;;;;;;;;;;;;;;;

(igen + ((word a) (word b))
    lda a
    clc
    adc b
    sta a
    lda (++ a)
    adc (++ b)
    sta (++ a))

(igen - ((word a) (word b))
    lda a
    sec
    sbc b
    sta a
    lda (++ a)
    sbc (++ b)
    sta (++ a))

;;;;;;;;;;;;;;;;;;;
;;; PROCEDURES ;;;;
;;;;;;;;;;;;;;;;;;;

(igen %proc-pro (name)
name:
    ,(!? (cdr (assoc name *fi*))
         (awhen !.nlocals
           (and (> ! 0)
                $(subsp # ,!)))))

(igen %proc-epi (name)
    ,(!? (cdr (assoc name *fi*))
         (awhen !.nlocals
           (and (> ! 0)
                $(addsp # ,!)))
    rts))
