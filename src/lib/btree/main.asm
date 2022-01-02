.export _bt_add, _bt_lookup

BT_LEFT     = 0
BT_RIGHT    = 3
BT_PARENT   = 6
BT_VALUE    = 9

    .zeropage

bt_p:   .res 3
r:      .res 3

    .code

.proc bt_alloc
.endproc

.proc bt_cmp_lt
.endproc

.proc bt_cmp_eq
.endproc

.proc bt_cmp_gt
.endproc

.proc bt_cmp_eq_parent_left
.endproc

;(defmethod bnode add (k v)
;  (? (< k key)
;     (_add-left k v)
;     (_add-right k v)))
.proc _bt_add
    jsr bt_cmp_lt
    bne bt_add_right
.endproc

;(defmethod bnode _add-left (k v)
;  (!? left
;      (!.add k v)
;      (= left (new bnode k v this))))

.proc bt_add_left
    ldy #BT_LEFT
    lda (bt_p),y
    bne _bt_add

    jsr bt_alloc
    lda r+2
    pha
    lda r+1
    pha
    lda r
    sta (bt_p),y
    iny
    pla
    sta (bt_p),y
    iny
    pla
    sta (bt_p),y
.endproc

;(defmethod bnode _add-right (k v)
;  (!? right
;      (!.add k v)
;      (= right (new bnode k v this))))
.proc bt_add_right
    ldy #BT_RIGHT
    bne bt_add_left
.endproc

;(defmethod bnode _lookup-left (k)
;  (!? left
;      (!.lookup k)))
.proc bt_lookup_left
    ldy #BT_LEFT
    lda (bt_p),y
    bne _bt_lookup
    rts
.endproc

;(defmethod bnode lookup (k)
;  (? (== key k)
;     this
;     (? (< k key )
;        (_lookup-left k)
;        (_lookup-right k))))
.proc _bt_lookup
    jsr bt_cmp_eq
    bne r
    jsr bt_cmp_lt
    bne bt_lookup_left
.endproc

;(defmethod bnode _lookup-right (k)
;  (!? right
;      (!.lookup k)))
.proc bt_lookup_right
    ldy #BT_LEFT
    lda (bt_p),y
    bne _bt_lookup
    rts
.endproc

;(defmethod bnode get-first ()
;  (!? left
;      (!.get-first)
;      this))
.proc bt_first
    ldy #BT_LEFT
    lda (bt_p),y
    bne bt_first
    rts
.endproc

;(defmethod bnode _next-trav-left (k)
;  (!? left
;      (!._next-trav k)
;      (? (> key k)
;         this
;         (_next-parent k))))
.proc bt_next_trav_left
    ldy #BT_LEFT
    lda (bt_p),y
    bne bt_next_trav
    jsr bt_cmp_gt
    beq bt_next_parent
    rts
.endproc

;(defmethod bnode _next-trav-right (k)
;  (!? right
;      (!._next-trav k)
;      (_next-parent k)))
.proc bt_next_trav_right
    ldy #BT_RIGHT
    lda (bt_p),y
    bne bt_next_trav
.endproc

;(defmethod bnode _next-parent (k)
;  (!? parent
;      (? (eq this parent.left)
;         (? (> !.key k)
;            !
;            (!._next-trav-right k)) ; !.key is k
;         (!._next-parent k))))
.proc bt_next_parent
    ldy #BT_PARENT
    lda (bt_p),y
    beq r
    jsr bt_cmp_eq_parent_left
    beq bt_next_parent
    jsr bt_cmp_gt
    beq bt_next_trav_right
r:  rts
.endproc

;(defmethod bnode _next-trav (k)
;  (? (<= key k)
;     (_next-trav-right k)
;     (_next-trav-left k)))
.proc bt_next_trav
    jsr bt_cmp_gt
    beq bt_next_trav_right
    bne bt_next_trav_left
.endproc

;(defmethod bnode next ()
;   (_next-trav key))
