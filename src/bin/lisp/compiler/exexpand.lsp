(load 'compiler/package.lsp)

(fn expex-move-arg (x)
  (? (cons? x)
     (!= (symbol)
       (. ! (list (list '= ! x))))
     (. x nil)))

; Returns:
; carlist: New arguments.
; cdrlist: Off-loaded expressions, assigning to temporaries.
(fn expex-move-args (x)
  (@ expex-move-arg x))

; Move function calls out of argument list and
; replace them by anonymous symbols.
(fn exexpand (x)
  (?
    (atom x)
      (list x)
    ; Expression in assigment
    (eq '= x.)
      (? (atom ..x.)
         (list x)
         (!= (expex-move-args (cdr ..x.))
           (append (mapcan exexpand (mapcan cdr !))
                   (list (list '= .x. (. (car ..x.) (@ car !)))))))
    ; Expression
    (!= (expex-move-args .x)
      (append (mapcan exexpand (mapcan cdr !))
              (list (. x. (@ car !)))))))

(in-package nil)
