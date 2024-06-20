(macro prog1 body
  ;"Return value of first expression."
  (let g (symbol)
    $(((,g)
         ,@(cdr body)
        ,g)
      ,(car body))))
