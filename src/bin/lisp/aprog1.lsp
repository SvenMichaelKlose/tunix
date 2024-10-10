(macro aprog1 (x . body)
  ;"Return value of first expression."
  $(let (! ,x)
     ,@body
     !))
