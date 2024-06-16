; Wrap expander in special form as it is recursive and needs
; to call itself with arguments evaluated.
(special quasiquote (qqx)
  (%qq qqx))

(fn %qq (qqx)
  (?
    ; End of list.
    (atom qqx)
      qqx

    ; Catch atoms to have to care about expressions only
    ; afterwards.
    (atom (car qqx))
      (cons (car qqx) (%qq (cdr qqx)))

    ; Replace UNQUOTE expression by its evaluated argument.
    (eq (caar qqx) 'unquote)
      (cons (eval (cadar qqx)) (%qq (cdr qqx)))

    ; Insert evaluated arguments of UNQUOTE-SPLICE into
    ; the list.
    (eq (caar qqx) 'unquote-spliced)
      (append (eval (cadar qqx)) (%qq (cdr qqx)))

    ; Just copy then...
    (cons (car qqx) (%qq (cdr qqx)))))

(print 'quasiquote)(terpri)
(print $(1 2 ,3 ,4))(terpri)
(print $(1 2 ,@'(3 4)))(terpri)
