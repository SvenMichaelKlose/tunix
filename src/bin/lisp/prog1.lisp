(macro prog1 body
  ;"Return value of first expression."
  (((g)
     $(((,g)
          ,@(cdr body)
          ,g)
       ,(car body)))
   (symbol)))
