(macro prog1 body
  ;"Return value of first expression."
  (let g (symbol)
    $(let ,g ,(car body)
       ,@(cdr body)
       ,g)))

(out "Testing PROG1...")(terpri)
(or (equal (eval (macroexpand '(prog1 1 2 3)))
           1)
    (error (eval (macroexpand '(prog1 1 2 3)))))
