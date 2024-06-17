(macro prog1 body
  ;"Return value of first expression."
  (let g (symbol)
    $(((,g)
        ,@(cdr body)
        ,g)
      ,(car body))))

(macro progn body
  ;"Return value of last expression."
  $(block t ,@body))

(print 'macroexpand)(terpri)
(or (equal (macroexpand '(progn (error)))
           '(block t (error))))
