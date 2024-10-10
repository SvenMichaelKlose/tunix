(macro != (x . body)
  $(with (! ,x)
     ,@body))
