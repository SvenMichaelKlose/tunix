.export _bt_add, _bt_lookup
.exportzp bt_p

BT_LEFT     = 0
BT_RIGHT    = 3
BT_PARENT   = 6
BT_VALUE    = 9

    .zeropage

bt_p:   .res 3
r:      .res 3

    .code

bt_alloc:
bt_cmp_lt:
bt_cmp_eq:
bt_cmp_gt:
bt_cmp_eq_parent_left:

step:
    lda (bt_p),y
    pha
    iny
    lda (bt_p),y
    pha
    iny
    lda (bt_p),y
    ldy #2
    sta (bt_p),y
    pla
    dey
    sta (bt_p),y
    pla
    dey
    sta (bt_p),y
    rts

;(defmethod bnode add (k v)
;  (? (< k key)
;     (_add-left k v)
;     (_add-right k v)))
_bt_add:
    jsr bt_cmp_lt
    bne bt_add_right

;(defmethod bnode _add-left (k v)
;  (!? left
;      (!.add k v)
;      (= left (new bnode k v this))))

bt_add_left:
    ldy #BT_LEFT
bt_add_left2:
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

;(defmethod bnode _add-right (k v)
;  (!? right
;      (!.add k v)
;      (= right (new bnode k v this))))
bt_add_right:
    ldy #BT_RIGHT
    bne bt_add_left2

;(defmethod bnode _lookup-left (k)
;  (!? left
;      (!.lookup k)))
bt_lookup_left:
    ldy #BT_LEFT
    lda (bt_p),y
    bne _bt_lookup
    rts

;(defmethod bnode lookup (k)
;  (? (== key k)
;     this
;     (? (< k key )
;        (_lookup-left k)
;        (_lookup-right k))))
_bt_lookup:
    jsr bt_cmp_eq
    beq r
    jsr bt_cmp_lt
    bne bt_lookup_left

;(defmethod bnode _lookup-right (k)
;  (!? right
;      (!.lookup k)))
bt_lookup_right:
    ldy #BT_LEFT
    lda (bt_p),y
    bne _bt_lookup
    rts

;(defmethod bnode get-first ()
;  (!? left
;      (!.get-first)
;      this))
bt_first:
    ldy #BT_LEFT
    lda (bt_p),y
    bne bt_first
    rts

;(defmethod bnode _next-trav-left (k)
;  (!? left
;      (!._next-trav k)
;      (? (> key k)
;         this
;         (_next-parent k))))
bt_next_trav_left:
    ldy #BT_LEFT
    lda (bt_p),y
    bne bt_next_trav
    jsr bt_cmp_gt
    beq bt_next_parent
    rts

;(defmethod bnode _next-trav-right (k)
;  (!? right
;      (!._next-trav k)
;      (_next-parent k)))
bt_next_trav_right:
    ldy #BT_RIGHT
    lda (bt_p),y
    bne bt_next_trav

;(defmethod bnode _next-parent (k)
;  (!? parent
;      (? (eq this parent.left)
;         (? (> !.key k)
;            !
;            (!._next-trav-right k)) ; !.key is k
;         (!._next-parent k))))
bt_next_parent:
    ldy #BT_PARENT
    lda (bt_p),y
    beq r2
    jsr bt_cmp_eq_parent_left
    beq bt_next_parent
    jsr bt_cmp_gt
    beq bt_next_trav_right
r2: rts

;(defmethod bnode _next-trav (k)
;  (? (<= key k)
;     (_next-trav-right k)
;     (_next-trav-left k)))
bt_next_trav:
    jsr bt_cmp_gt
    beq bt_next_trav_right
    bne bt_next_trav_left

;(defmethod bnode next ()
;   (_next-trav key))
