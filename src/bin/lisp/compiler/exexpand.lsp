(load 'compiler/package.lsp)
(require '!= 'mapcar 'mapcan)

; @ move-arg returns:
; carlist: New arguments.
; cdrlist: Off-loaded expressions, assigning to temporaries.
(fn move-arg (x)
  (? (cons? x)
     (!= (symbol)
       (. ! $((= ,! ,x))))
     (. x nil)))

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
         (!= (@ move-arg (cdr ..x.))
           (append (mapcan exexpand (mapcan cdr !))
                   $((= ,.x. (,(car ..x.) ,@(@ car !)))))))
    ; Expression
    (!= (@ move-arg .x)
      (append (mapcan exexpand (mapcan cdr !))
              $((,x. (@ car !)))))))

(in-package nil)
