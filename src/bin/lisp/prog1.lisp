(macro prog1 (x . body)
  ;"Return value of first expression."
  (let g (symbol)
    $(let ,g ,x
       ,@body
       ,g)))
