(fn expex-move-arg (x)
  (? (cons? x)
     (!= (symbol)
       (. ! (list (list '= ! x))))
     (. x nil)))

; Move function calls out of argument list and
; replace them by anonymous symbols.
(fn exexpand (x)
  (?
    (atom x)
      (list x)
    ; Handle assignments differently.
    (eq '= (car x))
      (? (atom (caddr x))
         ; Ignore atomic value.
         (list x)
         ; Do the real work.
         ; Split arguments into off-loaded assignments (cdrlist),
         ; and replacement or original symbols (carlist).
         (!= (@ expex-move-arg (cdr (caddr x)))
           ; Concatenate moved arguments...
           (append (mapcan exexpand (mapcan cdr !))
                   ; ...and re-assemble orgiginal assignment with new argument list.
                   (list (list '= (cadr x) (. (car (caddr x)) (carlist !)))))))
    ; Do the real work.
    (!= (@ expex-move-arg (cdr x))
      ; Concatenate moved arguments...
      (append (mapcan exexpand (mapcan cdr !))
              ; ...and re-assemble orgiginal expression with new argument list.
              (list (. (car x) (carlist !)))))))
