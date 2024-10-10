(macro aprog1 (x . body)
  ;"Return value of first expression."
  $(with (! ,x)
     ,@body
     !))
