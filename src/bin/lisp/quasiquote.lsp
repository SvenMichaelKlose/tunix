(or (builtin? 'append)
    (load "append.lsp"))

(fn %qeval (qqx)
  (eval qqx))

(fn %qq (qqx)
  (?
    ; End of list.
    (atom qqx)
      qqx

    ; Catch atoms.
    (atom (car qqx))
      (cons (car qqx) (%qq (cdr qqx)))

    ; Replace UNQUOTE expression by its evaluated argument.
    (eq (caar qqx) 'unquote)
      (cons (%qeval (cadar qqx)) (%qq (cdr qqx)))

    ; Insert evaluated argument of UNQUOTE-SPLICE into
    ; the list.
    (eq (caar qqx) 'unquote-spliced)
      (append (%qeval (cadar qqx)) (%qq (cdr qqx)))

    ; Just copy then...
    (cons (%qq (car qqx)) (%qq (cdr qqx)))))

; Wrap expander in special form as it is recursive and needs
; to call itself with arguments evaluated.
(special quasiquote (qqx)
  (%qq qqx))
