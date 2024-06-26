(macro prog1 body
  ;"Return value of first expression."
  (let g (symbol)
    $(let ,g ,(car body)
       ,@(cdr body))))
