(macro != (x . body)
  $(let (! ,x)
     ,@body))
