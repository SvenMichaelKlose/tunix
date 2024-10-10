(macro prog1 (x . body)
  ;"Return value of first expression."
  (with (g (symbol))
    $(with (,g ,x)
       ,@body
       ,g)))
