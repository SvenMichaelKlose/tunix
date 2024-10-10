(macro let-when (v x . body)
  $(with (,v ,x)
     (when ,v
       ,@body)))
