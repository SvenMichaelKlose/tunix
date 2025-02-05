(load 'compiler/package.lsp)
(require '!= 'mapcar '+@)

; @ move-arg returns:
; carlist: New arguments.
; cdrlist: Off-loaded expressions, assigning to temporaries.
(fn move-arg (x)
  (!= (symbol)
    (. ! $((= ,! ,x)))))

; Move arguments out of function call
; and assign them to temporaries.
(fn exexpand (x)
  (?
    (atom x)
      (list x)
    ; Expression in assigment
    (eq '= x.)
      (!= (@ move-arg (cdr ..x.))
        (+ (+@ exexpand (+@ cdr !))
           $((= ,.x. (,(car ..x.) ,@(@ car !))))))
    ; Expression
    (!= (@ move-arg .x)
      (+ (+@ exexpand (+@ cdr !))
         $((,x. (@ car !)))))))

(in-package nil)
