(fn %qqq (qqf qqx)
  (funcall qqf
           (? (and (cons? qqx)
                   (or (eq (cadar qqx)
                       'unquote)
                       (eq (cadar qqx)
                       'unquote-splice)))
                (. (cadar qqx)
                   (%qq (cddar qqx)))
                (eval (cadar qqx)))
           (%qq (cdr qqx))))

(fn %qq (qqx)
  (?
    ; End of list.
    (atom qqx) qqx

    ; Catch atoms.
    (atom (car qqx))
      (. (car qqx) (%qq (cdr qqx)))

    ; Replace UNQUOTE expression by its
    ; evaluated argument.
    (eq (caar qqx) 'unquote)
      (%qqq '. qqx)

    ; Insert evaluated argument of
    ; UNQUOTE-SPLICE into the list.
    (eq (caar qqx) 'unquote-spliced)
      ; TOOD: Test this with NCONC.
      (%qqq append qqx)

    ; Just copy then...
    (. (%qq (car qqx))
       (%qq (cdr qqx)))))

; Wrap expander in special form as it
; is recursive and needs to call itself
; with arguments evaluated.
(special quasiquote (qqx)
  (%qq qqx))
